(in-package :klacz)

(def class* worker-reactor (reactor)
  ())

(def function start-worker-reactor (worker-reactor)
  (run-reactor worker-reactor
	       :initial-bindings `((*standard-output* . ,*standard-output*)
				   (*error-output* . ,*error-output*))))

(def function stop-worker-reactor (worker-reactor)
  (call-reactor worker-reactor :quit))

(defgeneric call-bot-function (name reactor message line))

(defmethod call-bot-function (unknown irc-reactor message line)
  (call-reactor irc-reactor :reply-to message
		(format nil "Unknown function ~S called with arg ~S." unknown line)))

(defparameter *function-permissions* (make-hash-table))

(def reactor-hook :call ((reactor worker-reactor) irc-reactor message function line)
  (let* ((function-symbol (concatenate-symbol (string-upcase function) (find-package :keyword)))
	 (account (nick->account irc-reactor (source message)))
	 (place (reply-target message))
	 (level-needed (gethash function-symbol *function-permissions*)))
    (with-transaction
     (when (> level-needed 0)
       (let ((level (select-instance (l level) 
		      (where (and (eq (account-of l) account)
				  (eq (channel-of l) place))))))
	 (unless (and level (>= (level-of level) level-needed))
	   (call-reactor irc-reactor :reply-to message
			 (format nil "You do not have the right to call this function: ~A" function-symbol))
	   (return-from reactor-hook nil))))  
     (handler-case 
	 (trivial-timeout:with-timeout (10)
	   (call-bot-function function-symbol irc-reactor message line))
       (trivial-timeout:timeout-error ()
	 (call-reactor irc-reactor :reply-to message
		       (format nil "At ~A, function ~S timed out."
			       (format-timestring nil (now) :format *date-format*)
			       function)))
       (error (e)
	 (call-reactor irc-reactor :reply-to message
		       (format nil "At ~A, a ~S has been encountered: ~A" 
			       (format-timestring nil (now) :format *date-format*)
			       (class-name (class-of e))
			       e))
	 (call-reactor irc-reactor :new-error e))))))

(def definer bot-function (name-and/or-qualifiers lambda-list &body body)
  (bind (((:values name qualifiers) 
	  (if (listp name-and/or-qualifiers)
	      (values (car name-and/or-qualifiers) (cdr name-and/or-qualifiers))
	      (values name-and/or-qualifiers nil))))
    (with-gensyms (name-sym)
      `(progn
	 (defmethod call-bot-function ((,name-sym (eql ,name)) ,@lambda-list)
	   ,@body)
	 (setf (gethash ,name *function-permissions*)
	       ,(getf qualifiers :level 0))))))


(def macro with-arglist (lambda-list (line reactor message) &body body)
  (let ((rest-pos (position '&rest lambda-list)))
    (when (and rest-pos
	       (/= rest-pos (- (length lambda-list) 2)))
      (error "Malformed lambda list: misplaced ~S in ~S" 
	     '&rest lambda-list))
    (with-gensyms (args)
      `(let ((,args (rest (split "\\s+" ,line 
				 ,@(awhen (position '&rest lambda-list)
					  (list :limit (+ it 2)))))))
	 (if ,(if rest-pos
		  `(/= (length ,args) ,(1+ (position '&rest lambda-list)))
		   `(/= (length ,args) ,(length lambda-list)))
	     (call-reactor ,reactor :reply-to ,message 
			   (format nil "Given ~D arguments, but exactly ~D expected."
				   (length ,args) ,(if rest-pos
						       (1+ (position '&rest lambda-list))
						       (length lambda-list))))
	     (destructuring-bind ,(remove '&rest lambda-list) ,args
	       ,@body))))))

(def bot-function :say (irc-reactor message line)
  "Simply repeats the given line."
  (with-arglist (&rest to-say) (line irc-reactor message)
    (call-reactor irc-reactor :reply-to message to-say)))

(def bot-function :time (irc-reactor message line)
  "Prints current time."
  (call-reactor irc-reactor :reply-to message
		(format-timestring nil (now) :format *date-format*)))

(def bot-function :more (irc-reactor message line)
  (call-reactor irc-reactor :more message))

(defparameter *m8b-answers*
  '("As I see it, yes"
    "It is certain"
    "It is decidedly so"
    "Most likely"
    "Outlook good"
    "Signs point to yes"
    "Without a doubt"
    "Yes"
    "Yes - definitely"
    "You may rely on it"
    "Reply hazy, try again"
    "Ask again later"
    "Better not tell you now"
    "Cannot predict now"
    "Concentrate and ask again"
    "Don't count on it"
    "My reply is no"
    "My sources say no"
    "Outlook not so good"
    "Very doubtful"
    "You have AIDS anyway"))

(def bot-function :8b (irc-reactor message line)
  "Randomly answers to the given question."
  (call-reactor irc-reactor :reply-to message 
		(nth (random (length *m8b-answers*))
		     *m8b-answers*)))

(def bot-function :ping (irc-reactor message line)
  "Pings back the author of a message."
  (call-reactor irc-reactor :reply-to message 
		(format nil "~A: pong" (source message))))


(def bot-function :memo (irc-reactor message line)
  "Leaves user a memo."
  (with-arglist (to &rest memo) (line irc-reactor message)
    (make-instance 'memo 
		   :from (source message) 
		   :to to
		   :message memo)
    (call-reactor irc-reactor :reply-to message
		  (format nil "Added memo for ~S." to))))

(def bot-function :seen (irc-reactor message line)
  (with-arglist (who) (line irc-reactor message)
    (let ((last-seen (select-instance (s seen)
		       (where (eq (nickname-of s) who)))))
      (call-reactor irc-reactor :reply-to message
		    (if last-seen
			(with-slots (nickname date where kind message) last-seen
			  (format nil "Last seen ~A: ~A on ~A ~A ~A" 
				  nickname 
				  where
				  (format-timestring nil date :format *date-format*)
				  kind message))
			(format nil "Never seen ~A." who))))))


(def bot-function :identified? (irc-reactor message line)
  "Checks whether given nickname is identified."
  (let ((account (nick->account irc-reactor (source message))))
    (call-reactor irc-reactor :reply-to message
		  (if account
		      (format nil "~A is identified as ~A." (source message) account)
		      (format nil "~A is not identified." (source message))))))

(def bot-function :identify (irc-reactor message line)
  "Identifies user."
  (let ((nick (source message)))
    (start-identification irc-reactor nick)
    (call-reactor irc-reactor :reply-to message
		  (format nil "Attempting to identify ~A." nick))))

(def bot-function (:add-level :level 100) (irc-reactor message line)
  "Increases user privileges."
  (with-arglist (account channel new-level) (line irc-reactor message)
    (setf new-level (parse-integer new-level))
    (purge (l) (from (l level)) 
	   (where (and (eq (account-of l) account)
		       (eq (channel-of l) channel))))
    (make-instance 'level :account account :channel channel :level new-level)
    (call-reactor irc-reactor :reply-to message
		  (format nil "User ~A has now level ~D on channel ~A."
			  account new-level channel))))

(def bot-function (:kick :level 10) (irc-reactor message line)
  "Kicks given user."
  (with-arglist (nick &rest reason) (line irc-reactor message)
    (irc #'kick irc-reactor (reply-target message) nick reason)))
