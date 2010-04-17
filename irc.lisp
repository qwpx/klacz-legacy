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
  (add-hook *irc-connection* 'irc-quit-message #'log-hook))

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
  (ppcre:split ",?\\s+" args :limit (and rest-p arity)))

(defparameter *bot-functions* (make-hash-table :test 'equal))

(defun find-applicable-function (message-line)
  (when (char= #\, (aref message-line 0))
    (bind ((message-line (subseq message-line 1)))
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
     finally (progn 
               (when cdrs
                 (privmsg (connection message) 
                          (first (arguments message)) 
                          "But wait, there's more! (type ,more)"))
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
                   :message (second (arguments message)))))

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
        
                           