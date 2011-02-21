(in-package :klacz)

(hu.dwim.syntax-sugar:enable-sharp-l-syntax)

(def class* irc-reactor (reactor) 
  ((connection)
   (socket)
   (server)
   (port)
   (default-nickname)
   (default-channels)
   (nickserv-password)
   (more-lines nil)
   (identified-db (make-hash-table :test 'equal))
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
		     #L(call-reactor reactor reactor-hook !1)))) 
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


(def function reply-target (message)
  (let ((connection (connection message)))
    (if (string= (first (arguments message)) ;;check whether received on a channel or privmsg
		 (nickname (user connection))) 
	(source message)
	(first (arguments message)))))

(def reactor-hook :privmsg-lines ((reactor irc-reactor) target lines)
  (loop
     for cdrs on lines
     repeat *max-bot-lines*
     do (irc #'privmsg reactor target (car cdrs))
     finally (progn 
	       (when cdrs
		 (irc #'privmsg reactor target
		      (format nil "But wait, there's more! (~D more, type ,more)"
			      (length cdrs))))
	       (setf (more-lines-of reactor) cdrs))))

(def reactor-hook :reply-to ((reactor irc-reactor) message arg)
  (when (stringp arg)
    (setf arg (split-sequence:split-sequence #\Newline arg)))
  (call-reactor reactor :privmsg-lines
		(reply-target message)
		arg))

(def reactor-hook :more ((reactor irc-reactor) message)
  (call-reactor reactor :reply-to message (more-lines-of reactor)))

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
    :nickname "klacz"
    :channels '("#xyzzytest" "#qwpx" "#lisp-pl")
    :nickserv-password *nickserv-password*)

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


(def function create-seen-entry (nick where kind message)
  (purge (s) (from (s seen))
	 (where (eq (nickname-of s) nick)))
  (make-instance 'seen :nickname nick :where where :kind kind :message message))

(defparameter *message-regexp* (ppcre:create-scanner "^,\\s*(\\S+).*$"))

(def function parse-message-line (reactor message)
  (ppcre:register-groups-bind (function-name) (*message-regexp* (second (arguments message)))
    (call-reactor (worker-reactor-of reactor) :call
		  reactor message function-name (second (arguments message)))))

(def function maybe-memo (reactor message)
  (let* ((nick (source message))
	 (memos (select-instances (m memo)
		  (where (eq (sql-text "lower(_to)") (string-downcase nick)))))
	 (lines (list* (format nil "~A: You've got ~D new message~:P:"
			       (source message) (length memos))
		       (mapcar #L(format nil "From ~A at ~A: ~A"
					 (from-of !1)
					 (format-timestring nil (date-of !1)
							    :format *date-format*)
					 (message-of !1))
			       memos))))
    (when memos 
      (purge (m) (from (m memo))
	     (where (eq (sql-text "lower(_to)") (string-downcase nick))))
      (call-reactor reactor :reply-to message
		    lines))))

(defparameter *link-regexp* (ppcre:create-scanner "(\\w+://.*?)( |$)"))

(def function search-for-url-in-message (reactor message)
  (ppcre:register-groups-bind (link-text) (*link-regexp* (second (arguments message)))
    (aif (select-instance (l link)
	   (where (and (eq (channel-of l) (first (arguments message)))
		       (eq (link-of l) link-text))))
	 (let ((author (user-of it))
	       (date (format-timestring nil (date-of it) :format *date-format*))
	       (post-count (post-count-of it)))
	   (incf (post-count-of it))
	   (irc #'notice reactor (first (arguments message))
		   (format nil "That link has already been posted ~D time~:P (originally by ~A at ~A)"
			   post-count author date)))                             
	 (make-instance 'link
			:channel (first (arguments message))
			:user (source message)
			:link link-text))))



(def reactor-hook :irc-privmsg-message ((reactor qwpx-irc-connection) message)
  (with-transaction 
    (create-seen-entry (source message) (reply-target message) 
		       :privmsg (second (arguments message)))
    (search-for-url-in-message reactor message)
    (maybe-memo reactor message)
    (parse-message-line reactor message)))

(def reactor-hook :irc-join-message ((reactor qwpx-irc-connection) message)
  (with-transaction
    (start-identification reactor (source message))
    (create-seen-entry (source message) (reply-target message) 
		       :join "")))

(def reactor-hook :irc-part-message ((reactor qwpx-irc-connection) message)
  (with-transaction
    (unidentify reactor (source message))
    (create-seen-entry (source message) (reply-target message) 
		       :part (or (second (arguments message)) ""))))

(def reactor-hook :irc-quit-message ((reactor qwpx-irc-connection) message)
  (with-transaction
    (unidentify reactor (source message))
    (create-seen-entry (source message) "QUIT"
		       :quit (or (second (arguments message)) ""))))

(def reactor-hook :irc-rpl_whoisidentified-message ((reactor qwpx-irc-connection) message)
  (let ((nick (second (arguments message)))
	(account (third (arguments message))))
    (identify reactor nick account)))


(def reactor-hook :irc-rpl_endofnames-message ((reactor qwpx-irc-connection) message)
  (let* ((channel-name (second (arguments message)))
	 (channel (find-channel (connection-of reactor) channel-name)))
    (bordeaux-threads:make-thread (lambda ()
				    (maphash-keys #L(progn 
						      (start-identification reactor !1)
						      (sleep 1))
						  (users channel))))))

(def function start-identification (reactor nickname)
  (irc #'whois reactor nickname))

(def function identify (reactor nick account)
  (setf (gethash nick (identified-db-of reactor))
	  account))

(def function unidentify (reactor nick)
  (remhash nick (identified-db-of reactor)))

(def function nick->account (reactor nick)
  (gethash nick (identified-db-of reactor)))
