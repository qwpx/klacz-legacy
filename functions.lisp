(in-package :klacz)

(defparameter *irc-connection* nil
  "IRC connection")

(defparameter *channel* (make-instance 'chanl:unbounded-channel))
(defmacro within-irc (&body body)
  `(chanl:send *channel* 
               (lambda ()
                 ,@body)))

(defmethod author-of (message)
  (if (string= (first (arguments message)) *irc-nickname*)
      (source message)
      (first (arguments message))))

(defparameter *message-pool* nil)

(defmethod privmsg-lines (connection target (lines list))
  (flet ((escape-havoq-lol (line)
           (ppcre:regex-replace "^(\\s*)\\." line "\\1\\.")))
    (loop for cdrs on lines 
       repeat *max-bot-lines*
       do (privmsg connection 
                   target
                   (escape-havoq-lol (car cdrs)))
       finally (progn 
                 (when cdrs
                   (privmsg connection
                            target
                            (format nil "But wait, there's more! (~D more, type ,more)"
                                    (length cdrs)))
		   (setf *message-pool* cdrs))))))

(defmethod privmsg-lines (connection target (text string))
  (bind ((lines (ppcre:split "\\n+" text)))
    (privmsg-lines connection target lines)))

(defmethod reply-to (message lines)
  (privmsg-lines (connection message)
                 (if (string= (first (arguments message)) *irc-nickname*)
                     (source message)
                     (first (arguments message)))
                 lines))



(defmacro defbotf (&rest args)
  (flet ((parse-defbotf (cdr-of-form)
           (declare (list cdr-of-form))
           (let ((name (pop cdr-of-form))
                 (qualifiers ())
                 (spec-ll ()))
             (loop (if (and (car cdr-of-form) (atom (car cdr-of-form)))
                       (push (pop cdr-of-form) qualifiers)
                       (return (setq qualifiers (nreverse qualifiers)))))
             (setq spec-ll (pop cdr-of-form))
             (values name qualifiers spec-ll cdr-of-form))))
    (bind (((:values name qualifiers lambda-list body) (parse-defbotf args))
           ((&key (level 0)) qualifiers))
      `(progn 
         (defun ,(intern (concatenate 'string (string :bot-) (string name)))
             ,(remove '&rest lambda-list)
           ,@body)
         (setf (gethash (string ',name) *bot-functions*)
               (list (function ,(intern (concatenate 'string (string :bot-) 
                                                     (string name))))
                     ,(1- (length (remove '&rest lambda-list)))
                     ',(find '&rest lambda-list)
                     ,level))))))

(defparameter *bot-functions* (make-hash-table :test 'equal))

(defbotf say (message &rest text)
  "Writes given message to the source channel"
  (within-irc (reply-to message text)))

(defbotf + (message a b)
  "Adds two numbers and prints the result. 
Used mainly for testing purposes."
  (bind ((a (parse-integer a))
         (b (parse-integer b)))
    (within-irc (reply-to message (format nil "~D" (+ a b))))))

(defbotf more (message)
  "Prints queued output from the pool"
  (let ((lines *message-pool*)) ;; needs lexical binding
    (within-irc (reply-to message lines))
    (when (<= (length lines) *max-bot-lines*)
      (setf *message-pool* nil))))

(defbotf debug :level 10 (message)
	 "Invokes debugger"
	 (within-irc (reply-to message "Invoked debugger"))
	 (break))

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

(defbotf 8b (message &rest question)
  "Randomly answers to the given question."
  (declare (ignore question))
  (within-irc (reply-to message (nth (random (length *m8b-answers*))
                                     *m8b-answers*))))

(defbotf ping (message)
  "Pings back the author of a message."
  (within-irc 
    (reply-to message 
              (format nil "~A: pong" (source message)))))

(defbotf pick (message &rest arguments)
  "Sorts items in arbitrary order."
  (flet ((sum-vector (vector)
           (loop for n across vector sum n)))
    (bind ((words (ppcre:split "\\s+" arguments))
           (hashes (mapcar (lambda (w)
                             (list w (sum-vector 
                                      (ironclad:digest-sequence 
                                       :md5 (babel:string-to-octets w)))))
                           words)))
      (within-irc (reply-to message 
                            (format nil "~{~A~^ > ~}"
                                    (mapcar #'first (sort hashes #'> :key #'second))))))))

(defbotf seen (message nick)
  (with-transaction
    (bind ((last-entry (first (select-instances (l log-entry)
                                (where (and (eq (channel-of l) (first (arguments message)))
                                            (like (nick-of l) nick :case-sensitive-p nil)))
                                (order-by :descending (date-of l))
                                (limit 1))))
           (reply (if last-entry
                      (with-slots (nick date kind message) last-entry
                        (format nil "Last seen ~A: ~A ~A ~A" 
                                nick 
                                (format-timestring nil date :format *date-format*)
                                kind message))
                      (format nil "Never seen ~A." nick))))
      (within-irc 
        (reply-to message reply)))))


(defbotf memo (message nick &rest text)
  "Creates memo for another user."
  (with-transaction 
    (make-instance 'memo 
                   :channel (first (arguments message))
                   :from (source message)
                   :to nick
                   :message text))
  (within-irc 
    (reply-to message (format nil "Added memo for ~S." nick))))

(defbotf time (message)
  "Prints current time."
  (within-irc
    (reply-to message (format-timestring nil (now) :format *date-format*))))

(defbotf help (message)
  "Prints help."
  (bot-describe message "help"))

(defbotf add (message term-name &rest text)
  "Adds new entry to the database."
  (with-transaction
    (bind ((term (select-instance (t term)
                   (where (like (name-of t) term-name :case-sensitive-p nil)))))
      (unless term
        (setf term (make-instance 'term :name term-name))
        (within-irc 
          (reply-to message (format nil "Created term ~S." term-name))))
      (make-instance 'entry
                     :text text
                     :visible t
                     :term term
                     :added-by (source message))
      (unless (visible-p term) 
	(setf (visible-p term) t))
      (within-irc 
        (reply-to message (format nil "Added one entry to term ~S." 
                                  term-name))))))
(defmacro with-term ((var-name term-name message) &body body)
  `(bind ((,var-name (select-instance (term term)
                       (where (and (visible-p term) 
				   (like (name-of term) ,term-name :case-sensitive-p nil))))))
     (if (null ,var-name)
         (within-irc (reply-to ,message (format nil "Term ~S not found." ,term-name)))
         (progn 
           ,@body))))

(defbotf describe (message term-name)
  "Describes term."
  (with-transaction 
    (with-term (term term-name message)
      (bind ((n 0)
             (lines (list* (format nil "I heard ~S is:" term-name)
                           (mapcar (lambda (entry)
                                     (prog1
                                         (format nil "[~D] ~A" n (text-of entry))
                                       (incf n)))
                                   (select-instances (e entry)
                                     (where (and (eq (term-of e) term)
                                                 (eq (visible-p e) t)))
                                     (order-by :ascending (added-at-of e)))))))
        (within-irc (reply-to message lines))))))


(defmacro with-entry ((var term entry-number message) &body body)
  (bind ((term-name (gensym)))
    `(bind ((,var (first
                   (select-instances (e entry)
                     (where (and (eq (term-of e) ,term)
                                 (eq (visible-p e) t)))
                     (offset ,entry-number)
                     (limit 1)
                     (order-by :ascending (added-at-of e)))))
            (,term-name (name-of ,term)))
       (if ,var
           (progn 
             ,@body)
           (within-irc (reply-to ,message (format nil "No such entry in term ~S."
                                                  ,term-name)))))))


(defbotf forget :level 2 (message term-name entry-number)
	 "Removes entry from database"
	 (with-transaction
	   (with-term (term term-name message)
	     (bind ((clean-number (parse-integer entry-number)))
	       (with-entry (entry term clean-number message)
		 (setf (visible-p entry) nil)
		 (within-irc 
		   (reply-to message (format nil "Forgot ~:R entry of term ~S." 
					     clean-number term-name)))
		 (when (zerop (first (select ((count e)) (from (e entry)) 
					     (where (and (equal (term-of e) term)
							 (visible-p e))))))
		   (setf (visible-p term) nil)
		   (reply-to message (format nil "Forgot term ~S." term-name))))))))

(defbotf show-entry (message term-name entry-number)
  "Prints n-th entry of the given term."
  (with-transaction
    (with-term (term term-name message)
      (bind ((clean-number (parse-integer entry-number)))
	(with-entry (entry term clean-number message)
	  (bind ((text (text-of entry)))
	    (within-irc 
	      (reply-to message (format nil "[~D] ~A" clean-number text)))))))))


(defbotf random-entry (message term-name)
  "Prints random entry of a given term"
  (with-transaction
    (with-term (term term-name message)
      (bind ((count (first (select ((count e)) (from (e entry)) 
                                   (where (and (eq (term-of e) term)
                                               (eq (visible-p e) t)))))))
        (when count
          (bind ((entry (first (select-instances (e entry) 
                                 (where (and (eq (term-of e) term)
                                             (eq (visible-p e) t)))
                                 (offset (random count))
                                 (limit 1))))
                 (text (text-of entry)))
            (within-irc (reply-to message (format nil "~A" text)))))))))


(defbotf s :level 2 (message term-name entry-number &rest regexp)
	 "Performs regexp-replace in a given entry."
	 (with-transaction
	   (with-term (term term-name message) 
	     (bind ((clean-number (parse-integer entry-number)))
	       (with-entry (entry term clean-number message)
		 (or (ppcre:register-groups-bind (from to) ("^/((?:[^/\\\\]|\\\\.)+)/((?:[^/\\\\]|\\\\.)+)/$" regexp)
		       (bind (((:values result match-p) (ppcre:regex-replace-all from (text-of entry) to)))
			 (if match-p 
			     (progn 
			       (setf (text-of entry) result)
			       (within-irc (reply-to message 
						     (format nil "Replaced string in ~:R entry in term ~S."
							     clean-number term-name))))
			     (within-irc (reply-to message "No replacements performed.")))
			 t))
		     (within-irc
		       (reply-to message (format nil "Incorrect regexp.")))))))))





(defbotf eval :level 10 (message &rest code)
         "Evaluates given form and returns the result, along with its type."
         (bind ((out (make-string-output-stream))
		(result (let ((*package* (find-package :klacz-eval))
			      (*standard-output* out)
			      (*error-output* out))
                          (eval (read-from-string code)))))
           (within-irc
             (reply-to message 
                       (format nil "~A~%~S => ~A" (get-output-stream-string out) result (type-of result))))))


(defbotf describe-function (message function-name)
  "Returns description of a given function"
  (bind (((:values result found-p) (gethash (string-upcase function-name) *bot-functions*)))
    (if (not found-p)
        (within-irc (reply-to message (format nil "Unknown function: ~A" function-name)))
        (within-irc (reply-to message (format nil "~A" (with-output-to-string (s)
                                                         (describe (first result) s))))))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defbotf list-functions (message)
  "Prints all functions."
  (bind ((functions (sort (mapcar #'car (hash-table-alist *bot-functions*)) #'string<=))
         (groups (mapcar (lambda (list) (format nil "~{~A~^, ~}" list))
                         (group functions 10))))
    (within-irc (reply-to message groups))))

(defun join-strings (strings &optional (delimiter ""))
  (reduce (lambda (x y) (concatenate 'string x delimiter y))
	  strings))

(defbotf topic (message &rest text)
  "Enables user to set the part of the topic to his own message once in a while."
  (aif (nick-account (source message))
       (with-transaction
	 (let ((topic-change (first (select-instances (c topic-change)
				      (where (equal (user-of c) it)))))
	       (last-change (first (select-instances (c topic-change)
				     (order-by :descending (date-of c))
				     (limit 1))))
	       (time-now (now)))
	   (cond 
	     ((and last-change 
		   (timestamp>= (apply #'timestamp+ (date-of last-change) 
				       *min-topic-delay*)
				time-now))
	      (let ((last-date (date-of last-change)))
		(within-irc 
		  (reply-to message (format nil "Not enough time has passed since last topic change (was ~A)"
					    (format-timestring nil last-date :format *date-format*))))))
	     ((and topic-change
		   (timestamp>= (apply #'timestamp+ (date-of topic-change)
				       *min-topic-user-delay*)
				time-now))
	      (let ((last-date (date-of topic-change)))
		(within-irc 
		  (reply-to message (format nil "Not enough time has passed since ~A's last topic change (was ~A)"
					    it
					    (format-timestring nil last-date :format *date-format*))))))
	     ((ppcre:scan `(:group ,*topic-delimiter*) text)
	      (within-irc
		(reply-to message (format nil "New topic contains topic delimiter (~S)" *topic-delimiter*))))
	     (t
	      (let* ((channel-name (first (arguments message)))
		     (old-topic (topic (find-channel (connection message) channel-name)))
		     (topic-groups (ppcre:split `(:group ,*topic-delimiter*) old-topic))
		     new-topic)
		(setf (nth (1- (length topic-groups)) topic-groups) (concatenate 'string " " text)
		      new-topic (join-strings topic-groups *topic-delimiter*))
		(within-irc #-nil(topic- (connection message) channel-name new-topic)
			    #+nil(reply-to message new-topic))
		(make-instance 'topic-change :channel channel-name :text text :user it))))))
       (within-irc 
	 (reply-to message (format nil "This nick is unidentified: ~A. Try ,identify." (source message))))))

(let ((n 6))
  (defbotf sru (message)
    "Russian roulette, kicks a user with a chance one in six, otherwise prints some silly string."
    (if (zerop (random n))
	(progn 
	  (setf n 6)
	  (within-irc (kick *irc-connection* (first (arguments message)) (source message) 
			    "sru")))
	(progn 
	  (decf n 1)
	  (within-irc (reply-to message "You were lucky this time."))))))

(defbotf rpg-stats (message &rest nick)
  "Prints RPG stats of a given nick."
  (let* ((nick (if (string/= nick "") 
		   nick 
		   (source message)))
	 (stats (with-transaction
		  (select ((nick-of l) (count l)) 
			  (from (l log-entry)) 
			  (where (and
				  (not (equal nick (nick-of l)))
				  (not (equal "havoq" (nick-of l)))
				  (re-like (message-of l) (format nil ".*~A.*(\\+1|\\+\\+).*" nick) 
					  :case-sensitive-p nil)))
			  (group-by (nick-of l)))))
	 (stats-string (format nil "~D points (~{~{~A - ~D~}~^, ~})" 
			       (loop for stat in stats sum (second stat))
			       (sort stats #'> :key #'second))))
    (if (null stats)
	(within-irc (reply-to message "0 points, so sad."))
	(within-irc (reply-to message stats-string)))))
	


(defbotf kick :level 5 (message nick &rest reason)
  "Kicks a user with a given reason"
  (kick *irc-connection* (first (arguments message)) nick reason))

(defbotf op :level 11 (message nick)
  "Gives the operator privileges to a user."
  (op *irc-connection* (first (arguments message)) nick))

(defbotf create-poll (message name vote-limit &rest question)
  "Creates a poll with a given question and a vote limit."
  (with-transaction 
    (let ((vote-limit (parse-integer vote-limit :junk-allowed t)))
      (cond
	((select-instance (p poll) 
	   (where (equal name (name-of p))))
	 (within-irc (reply-to message
			       "Poll with a given name already exists.")))
	((or (not vote-limit) (not (plusp vote-limit)))
	 (within-irc (reply-to message
			       "Could not parse the vote limit.")))
	((null (nick-account (source message)))
	 (within-irc (reply-to message
			       "Your nick is not identified (try ,identify)")))
	(t (make-instance 'poll 
			  :name name 
			  :vote-limit vote-limit
			  :user (nick-account (source message))
			  :question question)
	   (within-irc (reply-to message		
				 (format nil "Poll ~A created." name))))))))

(defbotf describe-poll (message name)
  "Prints poll info: name, question and votes."
  (with-transaction
    (let ((poll (select-instance (p poll)
		  (where (equal name (name-of p))))))
      (if poll
	  (let* ((votes (mapcar #'user-of (votes-of poll)))
		 (poll-info (format nil "Poll ~A - ~S: votes ~D/~D (~
                                         ~{~A~^, ~})" 
				    (name-of poll)
				    (question-of poll)
				    (length votes)
				    (vote-limit-of poll)
				    votes)))
	    (within-irc (reply-to message poll-info)))
	  (within-irc (reply-to message 
				(format nil "No such poll: ~A" name)))))))

(defbotf vote (message name)
  "Votes for a given poll."
  (with-transaction
    (let ((poll (select-instance (p poll)
		  (where (equal name (name-of p)))))
	  (account (nick-account (source message))))
      (cond
	((null poll) 
	 (within-irc (reply-to message 
			       (format nil "There is no such poll: ~A." 
				       name))))
	((null account)
	 (within-irc (reply-to message 
			       "You're not identified (try ,identify)")))
	((select-instance (v vote) 
	   (where (and (equal (poll-of v) poll)
		       (equal (user-of v) account))))
	 (within-irc (reply-to message
			       "You already voted in this poll.")))
	((not (active-p poll))
	 (within-irc (reply-to message
			       "The poll has already ended.")))
	(t
	 (when (= (1+ (length (votes-of poll))) (vote-limit-of poll))
	   (setf (active-p poll) nil))
	 (make-instance 'vote :poll poll :user account)
	 (within-irc (reply-to message "Vote accepted.")))))))

(defbotf active-polls (message)
  (with-transaction 
    (let* ((polls (select-instances (p poll)
		    (where (active-p p))))
	   (lists (mapcar (lambda (names) (format nil "~{~A~^, ~}" names))
			  (group (mapcar #'name-of polls) 10))))
      (within-irc 
	(reply-to message "Active polls:")
	(reply-to message lists)))))
      
(defbotf print-base (message base number)
  (let* ((*print-base* (parse-integer base))
	 (number (princ-to-string (parse-integer number))))
    (within-irc 
      (reply-to message number))))
       

	
     