(in-package :klacz)

(defclass database-connection
    (hu.dwim.perec:postgresql/perec)
  ())

(defparameter hu.dwim.perec:*database*
  (make-instance 'database-connection
    :connection-specification
    `(:host ,*database-host*
      :database ,*database-name*
      :user-name ,*database-user*
      :password  ,*database-password*)))

(defparameter hu.dwim.perec::*compiled-query-cache* 
  (hu.dwim.perec::make-compiled-query-cache))

(defun ensure-db-in-sync ()
  (do-all-symbols (foo)
    (when (persistent-class-type-p foo)
      (hu.dwim.perec::export-to-rdbms (find-class foo)))))


(defpclass* term ()
  ((name :type (text 128) :unique t)
   (visible t :type boolean)))

(defpclass* entry () 
  ((text :type (text 256))
   (visible t :type boolean)
   (added-at (transaction-timestamp) :type timestamp)
   (added-by :type (text 64))))

(def persistent-association*
    ((:class term :slot entries :type (set entry))
     (:class entry :slot term :type term)))
   

(defpclass* log-entry ()
  ((channel :type (text 64))
   (kind :type (member action privmsg join part quit))
   (nick :type (text 64))
   (date (transaction-timestamp) :type timestamp :index t)
   (message :type (text 256))))

(defpclass* memo ()
  ((channel :type (text 64))
   (from :type (text 64))
   (to :type (text 64) :index t)
   (date (transaction-timestamp) :type timestamp)
   (active t :type boolean)
   (message :type (text 256))))

(defpclass* topic-change ()
  ((channel :type (text 64))
   (user :type (text 64))
   (text :type (text 256))
   (date (transaction-timestamp) :type timestamp)))

(defpclass* poll ()
  ((name :type (text 256))
   (question :type (text 256))
   (user :type (text 64))
   (date (transaction-timestamp) :type timestamp)
   (active t :type boolean)
   (vote-limit :type integer)))

(defpclass* vote ()
  ((user :type (text 64))
   (date (transaction-timestamp) :type timestamp)))

(def persistent-association*
    ((:class poll :slot votes :type (set vote))
     (:class vote :slot poll :type poll)))
   
(defpclass* link ()
  ((user :type (text 64))
   (channel :type (text 64))
   (date (transaction-timestamp) :type timestamp)
   (link :type (text 256) :unique t)
   (post-count 1 :type integer)))