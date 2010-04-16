(in-package :klacz)

(defparameter *irc-connection* nil
  "IRC connection")



(defparameter *irc-nickname* "klacz")
(defparameter *irc-server* "irc.freenode.net")
(defparameter *irc-channels* '("#xyzzytest" "#qwpx"))

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
  (add-hook *irc-connection* 'irc-privmsg-message #'privmsg-hook))

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
  (let ((result (ppcre:split "\\s+" line :limit 2)))
    (if (null (cdr result))
        (list (car result) "")
        result)))

(defun split-arguments (args arity rest-p)
  (when (and rest-p (zerop arity))
    (error "improper function definition"))
  (ppcre:split ",?\\s+" args :limit (and rest-p arity)))

(defparameter *bot-functions* (make-hash-table :test 'equal))

(defun find-applicable-function (message-line)
  (when (char= #\, (aref message-line 0))
    (let ((message-line (subseq message-line 1)))
      (bind (((function-name args-string) (parse-message-line message-line))
             ((:values result found-p) (gethash (string-upcase function-name)
                                                *bot-functions*)))
        (when found-p
          (bind (((function arity rest-p) result)
                 (args (split-arguments args-string arity rest-p)))
            (when (= (length args) arity)
              (values function args))))))))


(defmacro defbotf (name lambda-list &body body)
  `(progn 
     (defun ,(intern (concatenate 'string (string :bot-) (string name)))
         ,(remove '&rest lambda-list)
       ,@body)
     (setf (gethash (string ',name) *bot-functions*)
           (list (function ,(intern (concatenate 'string (string :bot-) 
                                                 (string name))))
                 ,(1- (length (remove '&rest lambda-list)))
                 ',(find '&rest lambda-list)))))


(defparameter *message-pool* nil)


(defmethod reply-to (message (lines list))
  (loop for cdrs on lines 
     repeat 4
     do (privmsg (connection message) 
                 (first (arguments message)) 
                 (car cdrs))
     finally (when cdrs
               (privmsg (connection message) 
                        (first (arguments message)) 
                        "But wait, there's more! (type ,more)")
               (setf *message-pool* cdrs))))

(defmethod reply-to (message (text string))
  (let ((lines (ppcre:split "\\n+" text)))
    (reply-to message lines)))

(defmacro with-irc (&body body)
  `(chanl:send *channel* 
               (lambda ()
                 ,@body)))

(defun handle-message (message)
  (with-simple-restart (continue "Continue processing IRC messages")
    (bind ((message-line (second (arguments message)))
           ((:values function args) (find-applicable-function message-line)))
      (when function
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

(defbotf say (message &rest text)
  (with-irc (reply-to message text)))

(defbotf + (message a b)
  (let ((a (parse-integer a))
        (b (parse-integer b)))
    (with-irc (reply-to message (format nil "~D" (+ a b))))))

(defbotf more (message)
  (with-irc (reply-to message *message-pool*)))

(defbotf debug (message)
  (with-irc (reply-to message "Invoked debugger"))
  (break))


(defbotf add (message term-name &rest text)
  (with-transaction
    (let ((term (select-instance (t term)
                  (where (eq (name-of t) term-name)))))
      (unless term
        (setf term (make-instance 'term :name term-name))
        (reply-to message (format nil "Created term \"~A\"." term-name)))
      (make-instance 'entry
                     :text text
                     :visible t
                     :term term
                     :added-by (source message))
      (with-irc 
          (reply-to message (format nil "Added one entry to term \"~A\"." 
                                    term-name))))))

(defmacro with-term ((var-name term-name message) &body body)
  `(let ((,var-name (select-instance (t term)
                      (where (eq (name-of t) ,term-name)))))
     (if (null ,var-name)
         (with-irc (reply-to ,message (format nil "Term \"~A\" not found." ,term-name)))
         (progn 
           ,@body))))

(defbotf describe (message term-name)
  (with-transaction 
    (with-term (term term-name message)
      (let* ((n 0)
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


(defbotf forget (message term-name entry-number)
  (with-transaction
    (with-term (term term-name message)
      (let* ((clean-number (parse-integer entry-number))
             (entry (first
                     (select-instances (e entry)
                       (where (and (eq (term-of e) term)
                                   (eq (visible-p e) t)))
                       (offset clean-number)
                       (limit 1)))))
        (if entry 
            (progn 
              (setf (visible-p entry) nil)
              (with-irc (reply-to message (format nil "Forgot ~:R entry of term \"~A\"." 
                                                  clean-number term-name))))
            (with-irc (reply-to message (format nil "No such entry in term \"~A\"."
                                                term-name))))))))


(defconstant +m8b-answers+ 
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
    "Very doubtful"))

(defbotf m8b (message)
  (with-irc (reply-to message (nth (random (length +m8b-answers+)) 
                                   +m8b-answers+))))