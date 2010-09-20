(in-package :klacz)

(def class* irc-reactor (reactor) 
  ((connection)
   (socket)
   (server)
   (port)
   (default-nickname)
   (default-channels)
   (nickserv-password)
   (identify-db (make-hash-table :test 'equal))))

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
  (with-slots (connection socket server port default-nickname default-channels) reactor
    (setf connection (connect :server server :port 6667 :nickname default-nickname)
	  socket (irc::socket connection))
    (setup-hook-mappings reactor connection)
    (nickserv-identify reactor)
    (mapc (curry #'join connection) default-channels)))

(defmethod read-next-call ((reactor irc-reactor))
  (with-slots (channel socket) reactor
    (loop for call = (or (usocket:wait-for-input socket :timeout 0.2 :ready-only t)
			 (chanl:recv channel :blockp nil))
	 until call
       finally (return
		 (if (eq (car call) socket)
		     (list :irc-message-ready nil)
		     call)))))

(def reactor-hook :irc-message-ready ((reactor irc-reactor))
  (read-message (connection-of reactor)))

(def reactor-hook :irc ((reactor irc-reactor) function &rest args)
  (apply function (connection-of reactor) args))

(def reactor-hook :quit-irc ((reactor irc-reactor) &optional message)
  (quit (connection-of reactor) message))



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
  (call-reactor *qwpx-irc-reactor* :quit-irc)
  (call-reactor *qwpx-irc-reactor* :quit)
  (setf *qwpx-irc-reactor* nil))







;; (defparameter *link-regexp* (ppcre:create-scanner "(\\w+://.*?)( |$)"))

;; (defmethod link-hook ((message irc-privmsg-message))
;;   (ppcre:register-groups-bind (link-text) (*link-regexp* (second (arguments message)))
;;     (with-transaction 
;;       (aif (select-instance (l link)
;; 	     (where (eq (link-of l) link-text)))
;; 	   (let ((author (user-of it))
;; 		 (date (format-timestring nil (date-of it) :format *date-format*))
;; 		 (post-count (post-count-of it)))
;; 	     (incf (post-count-of it))
;; 	     (within-irc 
;; 	       (notice *irc-connection* (author-of message)
;; 		       (format nil "That link has already been posted ~D time~:P (originally by ~A at ~A)"
;; 			       post-count author date))))			      
;; 	   (make-instance 'link
;; 			  :channel (first (arguments message))
;; 			  :user (source message)
;; 			  :link link-text)))))


