(in-package :klacz)

(defvar *database-lock* (bordeaux-threads:make-lock "database lock")
  "Lock for database writes.")

(def class locking-prevalence-system (guarded-prevalence-system)
  ((guard :initform (lambda (thunk)
		      (bordeaux-threads:with-lock-held (*database-lock*) 
			(funcall thunk))))))

(defvar *database* (make-prevalence-system *database-directory* 
					   :prevalence-system-class 'locking-prevalence-system)
  "Prevalence system.")

(def class* persistent-object-class (standard-class)
  ()
  (:documentation "Metaclass for persistent objects."))

(defmethod make-persistent-instance ((class persistent-object-class) &rest slotargs)
  (tx-create-object *database* class 
		    (loop for args on slotargs by #'cddr
		       collect (subseq args 0 2))))

(defmethod make-persistent-instance ((class symbol) &rest slotargs)
  (apply #'make-persistent-instance (find-class class) slotargs))

(defmethod validate-superclass ((class persistent-object-class) (superclass standard-class))
  "Yeah, they're compatible."
  t)


(def function all-objects (class)
  (find-all-objects *database* class))

(defgeneric select-instances (class function &key limit offset order-by order test-key order-key))

(defmethod select-instances ((instances list) function 
			     &key limit (offset 0) order-by order 
			     (test-key #'identity) (order-key #'identity))
  (loop 
     for object in instances
     
     when (funcall function (funcall test-key object))
     collect object into results

     finally (subseq  (if order-by
			  (sort results
				(ecase order
				  (:asc order-by)
				  (:desc (complement order-by)))
				:key order-key)
			  results)
		      offset (+ offset limit))))

(defmethod select-instances ((class persistent-object-class) function 
			     &rest args)
  (apply #'select-instances (all-objects class) function args))

(defmethod select-instances ((class symbol) function &rest args)
  (apply #'select-instances (find-class class) function args))

(defmethod select-instance-with-slot (class slot value &key test) 
  (find-object-with-slot *database* class slot value test))


(def class* term (object-with-id)
  ((name)
   (visible t))
  (:metaclass persistent-object-class)) 

(index-on *database* 'term '(name))

(def class* entry (object-with-id) 
  ((term-id :accessor get-term-id)
   (text)
   (visible t)
   (added-at (now))
   (added-by))
  (:metaclass persistent-object-class))


(defmethod term-of ((entry entry))
  (find-object-with-id *database* 'term (get-term-id entry)))

(defmethod entries-of ((term term))
  (remove (get-id term) (find-all-objects *database* 'entry) 
	  :key #'get-term-id :test-not #'eql))  

(def class* memo (object-with-id)
  ((channel)
   (from)
   (to)
   (date (now))
   (message))
  (:metaclass persistent-object-class))

(def class* topic-change (object-with-id)
  ((channel)
   (user)
   (text)
   (date (now)))
  (:metaclass persistent-object-class))
  
(def class* link (object-with-id)
  ((user)
   (channel)
   (date (now))
   (link)
   (post-count 1))
  (:metaclass persistent-object-class))

