(in-package :klacz)

(def class* reactor ()
  ((channel (make-instance 'chanl:unbounded-channel))))

(defgeneric call-reactor-hook (reactor hook args))

(defgeneric read-next-call (reactor))

(defmethod read-next-call ((reactor reactor))
  (chanl:recv (channel-of reactor)))

(defmethod run-reactor ((reactor reactor) &key (background t) 
			(initial-bindings nil))
  (flet ((reactor ()
	   (catch 'stop-reactor
	     (loop for (hook args) = (read-next-call reactor)
		do (with-simple-restart (continue-processing "Continue processing messages")
		     (call-reactor-hook reactor hook args))))))
    (if background
	(bordeaux-threads:make-thread #'reactor :initial-bindings initial-bindings)
	(bind (((symbols values) (unzip-alist initial-bindings)))
	  (progv symbols values
	    (reactor))))))

(def definer reactor-hook (hook-class lambda-list &body body)
  (with-gensyms (hook args)
    `(defmethod call-reactor-hook (,(car lambda-list) (,hook (eql,hook-class)) ,args)
       (destructuring-bind ,(cdr lambda-list) ,args
	 ,@body))))

(defgeneric call-reactor (reactor hook &rest args))

(defmethod call-reactor ((reactor reactor) hook &rest args)
  (chanl:send (channel-of reactor) (list hook args) :blockp nil))


(def reactor-hook :quit ((reactor reactor))
  (throw 'stop-reactor nil))

(def reactor-hook :call-later ((reactor reactor) function &rest args)
  (apply function args))

(def reactor-hook :unhandled-hook ((reactor reactor) hook-name args)
  (format *error-output* "UNHANDLED HOOK: ~S with args ~S on reactor ~S~%" hook-name args reactor))

(defmethod call-reactor-hook ((reactor reactor) hook args)
  (call-reactor reactor :unhandled-hook hook args))
