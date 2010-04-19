(in-package :klacz)

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
  (loop for cdrs on lines 
     repeat 4
     do (privmsg connection 
                 target
                 (car cdrs))
     finally (progn 
               (when cdrs
                 (privmsg connection
                          target
                          (format nil "But wait, there's more! (~D more, type ,more)"
                                  (length cdrs))))
               (setf *message-pool* cdrs))))

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
  (within-irc (reply-to message text)))

(defbotf + (message a b)
  (bind ((a (parse-integer a))
         (b (parse-integer b)))
    (within-irc (reply-to message (format nil "~D" (+ a b))))))

(defbotf more (message)
  (within-irc (reply-to message *message-pool*)))

(defbotf debug (message)
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
  (declare (ignore question))
  (within-irc (reply-to message (nth (random (length *m8b-answers*))
                                   *m8b-answers*))))

(defbotf ping (message)
  (within-irc 
    (reply-to message 
              (format nil "~A: pong" (source message)))))

(defbotf pick (message &rest arguments)
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
                                            (eq (nick-of l) nick)))
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
  (with-transaction 
    (make-instance 'memo 
                   :channel (first (arguments message))
                   :from (source message)
                   :to nick
                   :message text))
  (within-irc 
    (reply-to message (format nil "Added memo for \"~A\"." nick))))

(defbotf time (message)
  (within-irc
    (reply-to message (format-timestring nil (now) :format *date-format*))))

(defbotf help (message)
  (bot-describe message "help"))
        
(defbotf add (message term-name &rest text)
    (with-transaction
      (bind ((term (select-instance (t term)
                     (where (like (name-of t) term-name :case-sensitive-p nil)))))
        (unless term
          (setf term (make-instance 'term :name term-name))
          (within-irc 
            (reply-to message (format nil "Created term \"~A\"." term-name))))
        (make-instance 'entry
                       :text text
                       :visible t
                       :term term
                       :added-by (source message))
        (within-irc 
          (reply-to message (format nil "Added one entry to term \"~A\"." 
                                    term-name))))))
(defmacro with-term ((var-name term-name message) &body body)
  `(bind ((,var-name (select-instance (t term)
                       (where (like (name-of t) ,term-name :case-sensitive-p nil)))))
     (if (null ,var-name)
         (within-irc (reply-to ,message (format nil "Term \"~A\" not found." ,term-name)))
         (progn 
           ,@body))))

(defbotf describe (message term-name)
    (with-transaction 
      (with-term (term term-name message)
        (bind ((n 0)
               (lines (list* (format nil "I heard \"~A\" is:" term-name)
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
           (within-irc (reply-to ,message (format nil "No such entry in term \"~A\"."
                                                ,term-name)))))))


(defbotf forget :level 2 (message term-name entry-number)
    (with-transaction
      (with-term (term term-name message)
        (bind ((clean-number (parse-integer entry-number)))
          (with-entry (entry term clean-number message)
            (setf (visible-p entry) nil)
            (within-irc 
              (reply-to message (format nil "Forgot ~:R entry of term \"~A\"." 
                                        clean-number term-name))))))))


(defbotf random-entry (message term-name)
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


(defbotf s (message term-name entry-number regexp)
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
                                            (format nil "Replaced string in ~:R entry in term \"~A\"."
                                                    clean-number term-name))))
                      (within-irc (reply-to message "No replacements performed.")))
                  t))
              (within-irc
                (reply-to message (format nil "Incorrect regexp.")))))))))

