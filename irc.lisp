(in-package :klacz)



(defparameter *end-connection* nil)

(defparameter *irc-reader-thread* nil)
(defparameter *irc-writer-thread* nil)

(defparameter *identified-db* (make-hash-table :test #'equal))


(defun nickserv-identify ()
  (privmsg *irc-connection* "NickServ" 
	   (format nil "identify ~A" *nickserv-password*)))

(defun init-connection ()
  (clrhash *identified-db*)
  (setf *irc-connection*
        (connect :nickname *irc-nickname*
                 :server *irc-server*))
  (nickserv-identify)
  (mapc (curry #'join *irc-connection*) 
        *irc-channels*))

(defun privmsg-hook (message)
  (handle-message message))

(defun shuffle-hooks ()
  (add-hook *irc-connection* 'irc-privmsg-message #'privmsg-hook)
  (add-hook *irc-connection* 'irc-privmsg-message #'memo-hook)
  (add-hook *irc-connection* 'irc-privmsg-message #'link-hook)
  (add-hook *irc-connection* 'irc-privmsg-message #'log-hook)
  (add-hook *irc-connection* 'irc-rpl_endofnames-message #'identify-names-hook)
  (add-hook *irc-connection* 'irc-join-message #'log-hook)
  (add-hook *irc-connection* 'irc-part-message #'log-hook)
  (add-hook *irc-connection* 'irc-quit-message #'log-hook)
  (add-hook *irc-connection* 'irc-rpl_whoisidentified-message #'whoisidentified-hook)
  (add-hook *irc-connection* 'irc-kick-message #'unidentify-hook)
  (add-hook *irc-connection* 'irc-part-message #'unidentify-hook)
  (add-hook *irc-connection* 'irc-quit-message #'unidentify-hook)
  (add-hook *irc-connection* 'irc-join-message #'identify-hook))

(defun start-connection (&key (shuffle-hooks-p t))
  (flet ((reader-loop ()
           (loop for message =  (read-message *irc-connection*)
		while message))
         (writer-loop ()
           (loop until *end-connection*
              for f = (chanl:recv *channel*)
              do (funcall f))))
    (setf *end-connection* nil)
    (init-connection)
    (when shuffle-hooks-p
      (shuffle-hooks))
    (setf *irc-reader-thread*
          (bordeaux-threads:make-thread 
	   #'reader-loop 
	   :name "irc reader thread"
	   :initial-bindings (list (cons '*standard-output* 
					 *standard-output*)
				   (cons '*error-output*
					 *error-output*)))
          *irc-writer-thread* 
          (bordeaux-threads:make-thread 
	   #'writer-loop 
	   :name "irc writer thread"
	   :initial-bindings (list (cons '*standard-output* 
					 *standard-output*)
				   (cons '*error-output*
					 *error-output*))))
    t))

(defun end-connection ()
  (quit *irc-connection*)
  (chanl:send *channel* (lambda ()))
  (bordeaux-threads:join-thread *irc-reader-thread*)
  (bordeaux-threads:join-thread *irc-writer-thread*))

(defun parse-message-line (line)
  (bind ((result (ppcre:split "\\s+" line :limit 2)))
    (if (null (cdr result))
        (list (car result) "")
        result)))

(defun split-arguments (args arity rest-p)
  (when (and rest-p (zerop arity))
    (error "improper function definition"))
  (ppcre:split ",?\\s+" args :limit (when rest-p arity)))


(defun find-applicable-function (message-line)
  (when (and (not (zerop (length message-line)))
             (char= #\, (aref message-line 0)))
    (bind ((message-line (subseq message-line 1)))
      (bind (((function-name args-string) (parse-message-line message-line))
             ((:values result found-p) (gethash (string-upcase function-name)
                                                *bot-functions*)))
        (when found-p
          (bind (((function arity rest-p level) result)
                 (args (split-arguments args-string arity rest-p)))
            (when (= (length args) arity)
              (values function args level))))))))

(defun whitespace-char-p (char)
  (when (member char '(#\space #\tab #\newline))
    t))

(defun trim (string)
  (ppcre:regex-replace "^\\s*(.*?)\\s*$" string "\\1"))

(defun handle-message (message)
  (with-simple-restart (continue "Continue processing IRC messages")
    (bind ((message-line (trim (second (arguments message))))
           ((:values function args level) (find-applicable-function message-line)))
      (when (and (null function)
		 (not (zerop (length message-line)))
                 (char= (aref message-line 0) #\,))
        (setf function #'bot-random-entry
              args (list (subseq message-line 1))
              level 0))
      (when function
        (when (< (level-of (source message)) level)
          (within-irc 
            (reply-to message (format nil "You do not have right to call this function (your level is ~D, need ~D)"
                                      (level-of (source message)) level)))
          (return-from handle-message nil))
        (flet ((worker-function ()
                 (handler-bind 
		     ((error (lambda (var) 
			      (within-irc
				(reply-to message 
					  (format nil "At ~A, a ~S has been encountered: ~A" 
						  (format-timestring nil (now) :format *date-format*)
						  (class-name (class-of var))
						  var)))
			      (return-from worker-function nil)))
		      (warning #'muffle-warning))
		   (trivial-timeout:with-timeout (10)
		       (apply function (cons message args))))))
          (bordeaux-threads:make-thread #'worker-function                  
                                        :name "worker thread"
                                        :initial-bindings (list (cons '*standard-output* 
                                                                      *standard-output*)
                                                                (cons '*error-output*
                                                                      *error-output*))))))))
(defparameter *link-regexp* (ppcre:create-scanner "(\\w+://.*?)( |$)"))

(defmethod link-hook ((message irc-privmsg-message))
  (ppcre:register-groups-bind (link-text) (*link-regexp* (second (arguments message)))
    (with-transaction 
      (aif (select-instance (l link)
	     (where (eq (link-of l) link-text)))
	   (let ((author (user-of it))
		 (date (format-timestring nil (date-of it) :format *date-format*))
		 (post-count (post-count-of it)))
	     (incf (post-count-of it))
	     (within-irc 
	       (notice *irc-connection* (author-of message)
		       (format nil "That link has already been posted ~D time~:P (originally by ~A at ~A)"
			       post-count author date))))			      
	   (make-instance 'link
			  :channel (first (arguments message))
			  :user (source message)
			  :link link-text)))))


(defmethod log-hook ((message irc-privmsg-message))
  (with-transaction 
    (make-instance 'log-entry 
                   :channel (first (arguments message))
                   :kind 'privmsg
                   :nick (source message)
                   :message (second (arguments message)))))

(defmethod log-hook ((message irc-join-message))
  (with-transaction 
    (make-instance 'log-entry 
                   :channel (first (arguments message))
                   :kind 'join
                   :nick (source message)
                   :message "")))

(defmethod log-hook ((message irc-part-message))
  (with-transaction 
    (make-instance 'log-entry 
                   :channel (first (arguments message))
                   :kind 'part
                   :nick (source message)
                   :message (or (second (arguments message))
                                ""))))

(defmethod log-hook ((message irc-quit-message))
  (with-transaction 
    (make-instance 'log-entry 
                   :channel ""
                   :kind 'quit
                   :nick (source message)
                   :message (or (first (arguments message))
				""))))

(defmethod memo-hook ((message irc-privmsg-message))
  (with-transaction
    (bind ((memos (select-instances (m memo)
                    (where (and (active-p m)
                                (eq (to-of m) (source message))))))
           (lines (list* (format nil "~A: You've got ~D new message~:P:"
                                 (source message) (length memos))
                         (mapcar (lambda (memo)
                                   (format nil "From ~A at ~A: ~A"
                                           (from-of memo)
                                           (format-timestring nil (date-of memo)
                                                              :format *date-format*)
                                           (message-of memo)))
                                 memos))))
      (when memos
        (mapc (lambda (m) (setf (active-p m) nil)) memos)
        (within-irc 
          (reply-to message lines))))))






(defmethod unidentify-hook (message)
  (remhash (source message) *identified-db*))

(defmethod unidentify-hook ((message irc-kick-message))
  (remhash (second (arguments message)) *identified-db*))

(defmethod whoisidentified-hook ((message irc-rpl_whoisidentified-message))
  (setf (gethash (second (arguments message)) *identified-db*) 
        (third (arguments message))))

(defmethod identify-hook ((message irc-join-message))
  (identify-nick (source message)))

(defun identify-nick (nick)
  (whois *irc-connection* nick))

(defmethod identify-names-hook ((message irc-rpl_endofnames-message))
  )


(defbotf identify (message)
  "Identifies a user"
 (within-irc
   (identify-nick (source message))))

(defbotf identified-p (message)
  "Checks if a user is identified and returns the name of related account."
    (bind (((:values account found-p) (gethash (source message) *identified-db*)))
      (within-irc 
        (reply-to message
                  (if found-p
                      (format nil "~A is logged in as ~A (level ~D)" 
			      (source message) 
			      account 
			      (level-of (source message)))
                      (format nil "~A is not logged in" (source message)))))))

(defun nick-account (nick)
  (gethash nick *identified-db*))


(defun level-of (nick)
  (or (awhen (nick-account nick)
        (cdr (assoc it *acl* :test #'string-equal)))
       0))
