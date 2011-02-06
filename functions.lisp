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
  (let ((function-symbol (find-symbol (string-upcase function) (find-package :keyword))))
    (handler-case 
	(call-bot-function function-symbol irc-reactor message line)
      (error (e)
	(call-reactor irc-reactor :reply-to message
		      (format nil "At ~A, a ~S has been encountered: ~A" 
			      (format-timestring nil (now) :format *date-format*)
			      (class-name (class-of e))
			      e))
	(call-reactor irc-reactor :new-error e)))))

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


