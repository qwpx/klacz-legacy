(in-package :klacz)

(defparameter *irc-connection* nil
  "IRC connection")


(defparameter *end-connection* nil)

(defparameter *irc-reader-thread* nil)
(defparameter *irc-writer-thread* nil)
(defparameter *channel* (make-instance 'chanl:unbounded-channel))



(defun init-connection ()
  (setf *irc-connection*
        (connect :nickname *irc-nickname*
                 :server *irc-server*))
  (mapc (curry #'join *irc-connection*) 
        *irc-channels*))

(defun privmsg-hook (message)
  (handle-message message))

(defun shuffle-hooks ()
  (remove-hooks *irc-connection* 'irc-privmsg-message)
  (add-hook *irc-connection* 'irc-privmsg-message #'privmsg-hook)
  (add-hook *irc-connection* 'irc-privmsg-message #'memo-hook)
  (add-hook *irc-connection* 'irc-privmsg-message #'log-hook)
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
           (loop until *end-connection*
              while (read-message *irc-connection*)))
         (writer-loop ()
           (loop until *end-connection*
              for f = (chanl:recv *channel*)
              do (funcall f))))
    (setf *end-connection* nil)
    (init-connection)
    (when shuffle-hooks-p
      (shuffle-hooks))
    (setf *irc-reader-thread*
          (bordeaux-threads:make-thread #'reader-loop 
                                        :name "irc reader thread"
                                        :initial-bindings (list (cons '*standard-output* 
                                                                      *standard-output*)
                                                                (cons '*error-output*
                                                                      *error-output*)))
          *irc-writer-thread* 
          (bordeaux-threads:make-thread #'writer-loop 
                                        :name "irc writer thread"
                                        :initial-bindings (list (cons '*standard-output* 
                                                                      *standard-output*)
                                                                (cons '*error-output*
                                                                      *error-output*))))
    t))

(defun end-connection ()
  (setf *end-connection* t)
  (chanl:send *channel* (lambda ()))
  (bordeaux-threads:join-thread *irc-reader-thread*)
  (bordeaux-threads:join-thread *irc-writer-thread*)
  (quit *irc-connection*))

(defun parse-message-line (line)
  (bind ((result (ppcre:split "\\s+" line :limit 2)))
    (if (null (cdr result))
        (list (car result) "")
        result)))

(defun split-arguments (args arity rest-p)
  (when (and rest-p (zerop arity))
    (error "improper function definition"))
  (ppcre:split ",?\\s+" args :limit (when rest-p arity)))

(defparameter *bot-functions* (make-hash-table :test 'equal))

(defun find-applicable-function (message-line)
  (when (char= #\, (aref message-line 0))
    (bind ((message-line (subseq message-line 1)))
      (bind (((function-name args-string) (parse-message-line message-line))
             ((:values result found-p) (gethash (string-upcase function-name)
                                                *bot-functions*)))
        (when found-p
          (bind (((function arity rest-p level) result)
                 (args (split-arguments args-string arity rest-p)))
            (when (= (length args) arity)
              (values function args level))))))))


(defun parse-defbotf (cdr-of-form)
  (declare (list cdr-of-form))
  (let ((name (pop cdr-of-form))
        (qualifiers ())
        (spec-ll ()))
    (loop (if (and (car cdr-of-form) (atom (car cdr-of-form)))
              (push (pop cdr-of-form) qualifiers)
              (return (setq qualifiers (nreverse qualifiers)))))
    (setq spec-ll (pop cdr-of-form))
    (values name qualifiers spec-ll cdr-of-form)))  

(defmacro defbotf (&rest args)
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
                   ,level)))))


(defparameter *message-pool* nil)


(defmethod reply-to (message (lines list))
  (loop for cdrs on lines 
     repeat 4
     do (privmsg (connection message) 
                 (if (string= (first (arguments message)) *irc-nickname*)
                     (source message)
                     (first (arguments message)))
                 (car cdrs))
     finally (progn 
               (when cdrs
                 (privmsg (connection message) 
                          (if (string= (first (arguments message)) *irc-nickname*)
                              (source message)
                              (first (arguments message)))
                          (format nil "But wait, there's more! (~D more, type ,more)"
                                  (length cdrs))))
               (setf *message-pool* cdrs))))

(defmethod reply-to (message (text string))
  (bind ((lines (ppcre:split "\\n+" text)))
    (reply-to message lines)))

(defmacro with-irc (&body body)
  `(chanl:send *channel* 
               (lambda ()
                 ,@body)))

(defun handle-message (message)
  (with-simple-restart (continue "Continue processing IRC messages")
    (bind ((message-line (second (arguments message)))
           ((:values function args level) (find-applicable-function message-line)))
      (when (and (null function)
                 (char= (aref message-line 0) #\,))
        (setf function #'bot-random-entry
              args (list (subseq message-line 1))
              level 0))
      (when function
        (when (< (level-of (source message)) level)
          (with-irc 
            (reply-to message (format nil "You do not have right to call this function (your level is ~D, need ~D)"
                                      (level-of (source message)) level)))
          (return-from handle-message nil))
        (flet ((worker-function ()
                 (handler-case 
                     (apply function (cons message args))
                   (error (var) 
                     (with-irc
                       (reply-to message 
                                 (format nil "An error has been encountered: ~A" 
                                         var))))
                   (warning (var) 
                     (with-irc
                       (reply-to message (format nil "~A" var)))))))
          (bordeaux-threads:make-thread #'worker-function                  
                                        :name "worker thread"))))))

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
                   :message (first (arguments message)))))

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
        (with-irc 
          (reply-to message lines))))))


(defbotf add (message term-name &rest text)
    (with-transaction
      (bind ((term (select-instance (t term)
                     (where (like (name-of t) term-name :case-sensitive-p nil)))))
        (unless term
          (setf term (make-instance 'term :name term-name))
          (with-irc 
            (reply-to message (format nil "Created term \"~A\"." term-name))))
        (make-instance 'entry
                       :text text
                       :visible t
                       :term term
                       :added-by (source message))
        (with-irc 
          (reply-to message (format nil "Added one entry to term \"~A\"." 
                                    term-name))))))
(defmacro with-term ((var-name term-name message) &body body)
  `(bind ((,var-name (select-instance (t term)
                       (where (like (name-of t) ,term-name :case-sensitive-p nil)))))
     (if (null ,var-name)
         (with-irc (reply-to ,message (format nil "Term \"~A\" not found." ,term-name)))
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
          (with-irc (reply-to message lines))))))


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
           (with-irc (reply-to ,message (format nil "No such entry in term \"~A\"."
                                                ,term-name)))))))


(defbotf forget :level 2 (message term-name entry-number)
    (with-transaction
      (with-term (term term-name message)
        (bind ((clean-number (parse-integer entry-number)))
          (with-entry (entry term clean-number message)
            (setf (visible-p entry) nil)
            (with-irc 
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
              (with-irc (reply-to message (format nil "~A" text)))))))))


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
                        (with-irc (reply-to message 
                                            (format nil "Replaced string in ~:R entry in term \"~A\"."
                                                    clean-number term-name))))
                      (with-irc (reply-to message "No replacements performed.")))
                  t))
              (with-irc
                (reply-to message (format nil "Incorrect regexp.")))))))))



(defparameter *identified-db* (make-hash-table :test #'equal))

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
  (with-irc (whois *irc-connection* nick)))

(defbotf identify (message)
    (identify-nick (source message)))

(defbotf identified-p (message)
    (bind (((:values account found-p) (gethash (source message) *identified-db*)))
      (with-irc 
        (reply-to message
                  (if found-p
                      (format nil "~A is logged in as ~A" (source message) account)
                      (format nil "~A is not logged in" (source message)))))))

(defun nick-account (nick)
  (gethash nick *identified-db*))


(defparameter *acl*
  '(("Dodecki" . 9001)))

(defun level-of (nick)
  (or (awhen (nick-account nick)
        (cdr (assoc it *acl* :test #'string-equal)))
       0))

