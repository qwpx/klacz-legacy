(in-package :klacz)

(def class* irc-reactor (reactor) 
  ((connection)
   (socket)
   (server)
   (port)
   (default-nickname)
   (default-channels)
   (nickserv-password)
   (more-lines nil)
   (worker-reactor (make-instance 'worker-reactor))))

(defparameter *irc-hook-mappings* (make-hash-table :test 'eq))

(flet ((create-hook-mapping (message-class)
	 (setf (gethash (concatenate-symbol :irc- message-class :-message 
					    (find-package :keyword))
			*irc-hook-mappings*)
	       (concatenate-symbol :irc- message-class :-message 
				   (find-package :irc)))))
  (mapc #'create-hook-mapping 
	'(:privmsg :notice :kick :topic :error :mode :ping
		  :nick :join :part :quit :kill :pong :invite))
  (mapc #'create-hook-mapping (mapcar #'second irc::*reply-names*)))


(defmethod setup-hook-mappings ((reactor irc-reactor) (connection connection))
  (flet ((map-hook (reactor-hook irc-hook)
	   (add-hook connection irc-hook 
		     (lambda (message)
		       (call-reactor reactor reactor-hook message))))) 
    (maphash #'map-hook *irc-hook-mappings*)))

(defmethod run-reactor :before ((reactor irc-reactor) &rest args)
  (declare (ignore args))
  (with-slots (connection socket server port default-nickname default-channels worker-reactor) reactor
    (start-worker-reactor worker-reactor)
    (setf connection (connect :server server :port port :nickname default-nickname)
	  socket (irc::socket connection))
    (setup-hook-mappings reactor connection)
    (nickserv-identify reactor)
    (mapc (curry #'join connection) default-channels)
    (flet ((irc-listener ()
	     (handler-case 
		 (loop for input = (usocket:wait-for-input socket :ready-only t)
		    when input
		    do (read-message connection))
	       (sb-int:closed-stream-error () 
		 ;; ignore
		 nil))))
	(bordeaux-threads:make-thread #'irc-listener
				      :name (format nil "IRC-LISTENER-FOR-~A"
						    (class-name (class-of reactor)))))))

(def reactor-hook :irc ((reactor irc-reactor) function &rest args)
  (apply function (connection-of reactor) args))

(def function irc (function reactor &rest args)
  (apply #'call-reactor reactor :irc function args))

(def reactor-hook :quit ((reactor irc-reactor) &optional message)
  (quit (connection-of reactor) message)
  (stop-worker-reactor (worker-reactor-of reactor))
  (call-next-method))


(def function reply-target (connection message)
  (if (string= (first (arguments message)) ;;check whether received on a channel or privmsg
	       (nickname (user connection))) 
      (source message)
      (first (arguments message))))

(def reactor-hook :reply-to ((reactor irc-reactor) message arg)
  (when (stringp arg)
    (setf arg (split-sequence:split-sequence #\Newline arg)))
  (loop
     with connection = (connection-of reactor)
     with target = (reply-target connection message)
     for cdrs on arg 
     repeat *max-bot-lines*
     do (irc #'privmsg reactor target (car cdrs))
     finally (progn 
	       (when cdrs
		 (irc #'privmsg reactor target
			       (format nil "But wait, there's more! (~D more, type ,more)"
				       (length cdrs)))
		 (setf (more-lines-of reactor) cdrs)))))

(def reactor-hook :privmsg ((reactor irc-reactor) target msg)
  (privmsg (connection-of reactor) target msg))


(def definer irc-connection (name &key server (port 6667) channels nickname nickserv-password)
  `(def class* ,name (irc-reactor)
     ((server ,server)
      (port ,port)
      (default-nickname ,nickname)
      (default-channels ,channels)
      (nickserv-password ,nickserv-password))))


(def function nickserv-identify (reactor)
  (awhen (nickserv-password-of reactor)
    (call-reactor reactor :irc #'privmsg 
		  "NickServ" (format nil "identify ~A" it))))


(def irc-connection qwpx-irc-connection
    :server "irc.freenode.net"
    :nickname "klacz-test"
    :channels '("#xyzzytest"))

(defvar *qwpx-irc-reactor*)

(def function start-qwpx-connection (&key (background t))
  (setf *qwpx-irc-reactor* (make-instance 'qwpx-irc-connection))
  (run-reactor *qwpx-irc-reactor* 
	       :background background
	       :initial-bindings `((*standard-output* . ,*standard-output*)
				   (*error-output* . ,*error-output*))))

(def function stop-qwpx-connection ()
  (call-reactor *qwpx-irc-reactor* :quit)
  (setf *qwpx-irc-reactor* nil))

(defparameter *message-regexp* (ppcre:create-scanner "^,\\s*(\\S+).*$"))

(def reactor-hook :irc-privmsg-message ((reactor qwpx-irc-connection) message)
  (ppcre:register-groups-bind (function-name) (*message-regexp* (trailing-argument message))
    (call-reactor (worker-reactor-of reactor) :call
		  reactor message function-name (trailing-argument message))))
