(in-package :klacz)

(hu.dwim.syntax-sugar:enable-sharp-l-syntax)

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
  (let ((space-pos (position #\Space line)))
    (if (and space-pos
	     (position #\Space line :test (compose #'not #'eql) :start space-pos))
	(call-reactor irc-reactor :reply-to message
		      (format nil "Unknown function ~S called with arg ~S." unknown line))
	(random-entry irc-reactor message (string unknown) ))))

(defparameter *function-permissions* (make-hash-table))

(def reactor-hook :call ((reactor worker-reactor) irc-reactor message function line)
  (let* ((function-symbol (concatenate-symbol (string-upcase function) (find-package :keyword)))
	 (account (nick->account irc-reactor (source message)))
	 (place (reply-target message))
	 (level-needed (gethash function-symbol *function-permissions*)))
    (with-transaction
      (when (> (or level-needed 0) 0)
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


(def definer bot-function-alias (name new-name)
     (with-gensyms (reactor message line)
       `(progn
	  (def bot-function ,new-name (,reactor ,message ,line)
	       ,(format nil "~A is an alias for ~A" new-name name)
	       (call-bot-function ,name ,reactor ,message ,line))
	  (setf (gethash ,new-name *function-permissions*)
		(gethash ,name *function-permissions* 0)))))

(def bot-function-alias :kick :sage)

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
			   (format nil "Given ~D argument~:P, but exactly ~D expected."
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
  "Increases user privileges, syntax: ,add-level account channel new-level."
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
    (when (string-equal nick (default-nickname-of irc-reactor))
      (setf nick (source message)
	    reason "NA PANA REKE PODNOSISZ, POLSKI PSIE?"))
    (irc #'kick irc-reactor (reply-target message) nick reason)))

(def bot-function (:eval :level 5) (irc-reactor message line)
  (with-arglist (&rest code) (line irc-reactor message)
    (let* ((out (make-string-output-stream))
	   (result (let* ((*standard-output* out)
			  (*error-output* out)
			  (*package* (find-package :klacz-eval))
			  (* nil) (+ nil) (/ nil)
			  (form (com.informatimago.common-lisp.lisp-reader.reader:read-from-string code)))
		     (eval form))))
      (call-reactor irc-reactor :reply-to message
		    (format nil "~A~%~S => ~A" (get-output-stream-string out) result (type-of result))))))
     
(def bot-function (:eval-klacz :level 100) (irc-reactor message line)
  (with-arglist (&rest code) (line irc-reactor message)
    (let* ((out (make-string-output-stream))
	   (result (let* ((*standard-output* out)
			  (*error-output* out)
			  (form (read-from-string code)))
		     (eval form))))
      (call-reactor irc-reactor :reply-to message
		    (format nil "~A~%~S => ~A" (get-output-stream-string out) result (type-of result))))))

(def bot-function :pick (irc-reactor message line)
  "Sorts items in arbitrary order."
  (flet ((sum-vector (vector)
           (loop for n across vector sum n)))
    (with-arglist (&rest to-pick) (line irc-reactor message)
      (let* ((words (ppcre:split "\\s+" to-pick))
	     (hashes (mapcar (lambda (w)
			       (list w (sum-vector 
					(ironclad:digest-sequence 
					 :md5 (babel:string-to-octets w)))))
			     words)))
	(call-reactor irc-reactor :reply-to message 
		      (format nil "~{~A~^ > ~}"
			      (mapcar #'first (sort hashes #'> :key #'second))))))))


(def bot-function :my-levels (irc-reactor message line)
  (let* ((account (nick->account irc-reactor (source message)))
	 (levels (select-instances (l level) 
		  (where (eq (account-of l) account)))))
    (call-reactor irc-reactor :reply-to message
		  (format nil "Your levels: ~{~{~A: ~A~}~^, ~}"
			  (mapcar #L(list (channel-of !1) (level-of !1))
				  levels)))))

(def bot-function (:topic :level 1) (irc-reactor message line)
  (with-arglist (&rest text) (line irc-reactor message)
    (let* ((account (nick->account irc-reactor (source message)))
	   (channel-name (first (arguments message)))
	   (topic-change (select-instance (c topic-change)
			   (where (and (eq (user-of c) account)
				       (eq (channel-of c) channel-name)
				       (eq (date-of c) 
					   (select ((max (date-of tc)))
					     (from (tc topic-change))
					     (where (and (eq (user-of tc) account)
							 (eq (channel-of tc) channel-name)))))))))
	   (last-change (select-instance (c topic-change)
			  (where (and (eq (channel-of c) channel-name)
				      (eq (date-of c)
					  (select ((max (date-of tc)))
					    (from (tc topic-change))
					    (where (eq (channel-of tc) channel-name))))))))
	   (time-now (now)))
      (cond 
	;; topic was recently changed
	((and last-change 
	      (timestamp>= (apply #'timestamp+ (date-of last-change) 
				  *min-topic-delay*)
			   time-now))
              (let ((last-date (date-of last-change)))
                (call-reactor irc-reactor :reply-to message 
			      (format nil "Not enough time has passed since last topic change (was ~A)"
				      (format-timestring nil last-date :format *date-format*)))))
	;; user recently changed topic
	((and topic-change
	      (timestamp>= (apply #'timestamp+ (date-of topic-change)
				  *min-topic-user-delay*)
			   time-now))
	 (let ((last-date (date-of topic-change)))
	   (call-reactor irc-reactor :reply-to message 
			 (format nil "Not enough time has passed since ~A's last topic change (was ~A)"
				 account
				 (format-timestring nil last-date :format *date-format*)))))
	;; topic contains delimiter
	((ppcre:scan `(:group ,*topic-delimiter*) text)
	 (call-reactor irc-reactor :reply-to message 
		       (format nil "New topic contains topic delimiter (~S)" *topic-delimiter*)))
	;; ok, we're changing topic
	(t
	 (let* ((old-topic (topic (find-channel (connection message) channel-name)))
		(topic-groups (ppcre:split `(:group ,*topic-delimiter*) old-topic))
		new-topic)
	   (setf (nth (1- (length topic-groups)) topic-groups) (concatenate 'string " " text)
		 new-topic  (join-strings topic-groups *topic-delimiter*))
	   (irc #'topic- irc-reactor channel-name new-topic)
	   (make-instance 'topic-change :channel channel-name :text text :user account)))))))

(def macro with-term ((var-name) (term-name irc-reactor message) &body body)
  `(let ((,var-name (select-instance (term term)
		      (where (and (visible-p term) 
				  (eq (sql-text "lower(_name)")
				      (string-downcase ,term-name)))))))
     (if (null ,var-name)
         (call-reactor ,irc-reactor :reply-to ,message
		       (format nil "Term ~S not found." ,term-name))
         (progn 
           ,@body))))


(def bot-function :describe (irc-reactor message line)
  "Describes term."
  (with-arglist (term-name) (line irc-reactor message)
    (with-term (term) (term-name irc-reactor message)
      (let* ((n 0)
	     (lines (list* (format nil "I heard ~S is:" (name-of term))
			   (mapcar #L(prog1
					 (format nil "[~D] ~A" n (text-of !1))
				       (incf n))
				   (select-instances (e entry)
				     (where (and (eq (term-of e) term)
						 (eq (visible-p e) t)))
				     (order-by :ascending (added-at-of e)))))))
	(call-reactor irc-reactor :reply-to message 
		      lines)))))

(def function random-entry (irc-reactor message term-name)
  (with-term (term) (term-name irc-reactor message)
      (let ((entry-count (first (select ((count e)) (from (e entry)) 
					(where (and (eq (term-of e) term)
						    (eq (visible-p e) t)))))))
	(when entry-count
	  (let* ((entry (first (select-instances (e entry) 
				(where (and (eq (term-of e) term)
					    (eq (visible-p e) t)))
				(offset (random entry-count))
				(limit 1))))
		 (text (text-of entry)))
	    (call-reactor irc-reactor :reply-to message
			  text))))))


(def bot-function :random-entry (irc-reactor message line)
  "Prints randomly chosen entry."
  (with-arglist (term-name) (line irc-reactor message)
    (random-entry irc-reactor message term-name)))

(def bot-function :add (irc-reactor message line)
  "Adds new entry to term, possibly creating the term."
  (with-arglist (term-name &rest entry-text) (line irc-reactor message)
    (let ((term (select-instance (term term)
		  (where (eq (sql-text "lower(_name)")
			     (string-downcase term-name))))))
      (unless term
	(setf term (make-instance 'term :name term-name))
	(call-reactor irc-reactor :reply-to message
		      (format nil "Created term ~S" term-name)))
      (unless (visible-p term)
	(setf (visible-p term) t))
      (make-instance 'entry 
		     :term term
		     :text entry-text
		     :added-by (source message))
      (call-reactor irc-reactor :reply-to message
		    (format nil "Added one entry to term ~S"
			    term-name)))))

(let ((n 6))
  (def bot-function :sru (irc-reactor message line)
    (if (zerop (random n))
	(progn
	  (irc #'kick irc-reactor (first (arguments message)) (source message))
	  (setf n 6))
	(progn
	  (call-reactor irc-reactor :reply-to message
			(format nil "You were lucky this time. (chance 1/~D)" n))
	  (decf n)))))

(def bot-function (:remove-term :level 10) (irc-reactor message line)
  "Removes term altogether, including all entries."
  (with-arglist (term-name) (line irc-reactor message)
    (with-term (term) (term-name irc-reactor message)
      (let ((name-of-term (name-of term)))
	(mapc #'purge-instance
	      (select-instances (e entry)
		(where (eq (term-of e) term))))
	(purge-instance term)
	(call-reactor irc-reactor :reply-to message
		      (format nil "Removed term ~A." name-of-term))))))

(def bot-function (:op :level 20) (irc-reactor message line)
  "Grants operator status to a user."
  (with-arglist (nick) (line irc-reactor message)
    (irc #'op irc-reactor (first (arguments message)) nick)))

(def bot-function :describe-function (irc-reactor message line)
  "Describes function."
  (with-arglist (function-name) (line irc-reactor message)
    (let ((function-symbol (concatenate-symbol function-name (find-package :keyword))))
      (call-reactor irc-reactor :reply-to message
		    (aif (gethash function-symbol *function-permissions*)
			 (format nil "Function ~A: Required level: ~D.~%Documentation: ~A" 
				 function-symbol it
				 (documentation (find-method #'call-bot-function () `((eql ,function-symbol) t t t)) t))
			 (format nil "Unknown function: ~A." function-symbol))))))
	   
(def bot-function :list-functions (irc-reactor message line)
  "Prints the list of bot functions."
  (let ((functions (hash-table-keys *function-permissions*)))
    (call-reactor irc-reactor :reply-to message
		  (format nil "~{~{~A~^, ~}~^~%~}"
			  (group functions 10)))))

(defun find-advice-images () 
  (let ((pathnames (directory (merge-pathnames *advice-images-path* "*.jpg"))))
    (mapcar (lambda (path)
	      (cons (pathname-name path) (file-namestring path)))
	    pathnames)))


(def bot-function :advice (irc-reactor message line)
  "Creates advice image, syntax: ,advice nick \"top\" \"bottom\""
  (with-arglist (name &rest advice) (line irc-reactor message)
    (let ((advice-regexp (ppcre:create-scanner "\\s*\"(.*?)\"\\s+\"(.*?)\"\\s*"))
	  (image (cdr (assoc name (find-advice-images) :test #'string-equal)))
	  (out-name (format nil "~A.jpg" (now))))
      (if (not (ppcre:scan advice-regexp advice))
	  (call-reactor irc-reactor :reply-to message
			"invalid syntax, expected ,advice nick \"top\" \"bottom\"")
	  (ppcre:do-register-groups (top bottom) (advice-regexp advice)
	    (if (not image)
		(call-reactor irc-reactor :reply-to message
			      (format nil "Unknown name: ~A" name))
		(progn 
		  (trivial-shell:shell-command 
		   (format nil "cd ~A && ./advice.sh advice/~A ~S ~S ~A"
			   *klacz-path* image (string-upcase top) (string-upcase bottom)
			   (merge-pathnames *advice-path* out-name)))
		  (make-instance 'advice :name name :image-name out-name :top top :bottom bottom)
		  (call-reactor irc-reactor :reply-to message 
				(format nil "~A~A" *advice-url* out-name)))))))))


(def bot-function :advice-images (irc-reactor message line)
  "Lists advice images"
  (call-reactor irc-reactor :reply-to message
		(format nil "Available advice images: ~{~A~^, ~}."
			(mapcar #'car (find-advice-images)))))

(def bot-function :math (irc-reactor message line)
  "Posts equation to mathbin"
  (with-arglist (&rest math) (line irc-reactor message)
      (multiple-value-bind (body status headers uri stream must-close reason)
	  (drakma:http-request "http://mathbin.net/index.html"
			       :parameters `(("title" . "#qwpx")
					     ("name" . "klacz")
					     ("save" . "1")
					     ("id" . "")
					     ("body" . ,(format nil "[EQ]~A[/EQ]" math))))
	(declare (ignore body status headers stream must-close reason))
	(let ((n (subseq (puri:uri-path uri) 1)))
	  (setf (puri:uri-path uri) (format nil "/equations/~A_0.png" n))
	  (call-reactor irc-reactor :reply-to message
			(puri:render-uri uri nil))))))
