(in-package :klacz)

(defparameter *database-name* "klacz")
(defparameter *database-user* "klacz")
(defparameter *database-password* "klacz")

(defclass database-connection
    (hu.dwim.perec:postgresql/perec)
  ())

(defparameter hu.dwim.perec:*database*
  (make-instance 'database-connection
    :connection-specification
    `(:database ,*database-name*
      :user-name ,*database-user*
      :password  ,*database-password*)))

(defparameter hu.dwim.perec::*compiled-query-cache* 
  (hu.dwim.perec::make-compiled-query-cache))

(defun ensure-db-in-sync ()
  (do-all-symbols (foo)
    (when (persistent-class-type-p foo)
      (hu.dwim.perec::export-to-rdbms (find-class foo)))))


(defpclass* term ()
  ((name :type (text 128) :unique t)))

(defpclass* entry () 
  ((text :type (text 256))
   (visible :type boolean)
   (added-at (transaction-timestamp) :type timestamp)
   (added-by :type (text 64))))

(def persistent-association*
    ((:class term :slot entries :type (set entry))
     (:class entry :slot term :type term)))
   
