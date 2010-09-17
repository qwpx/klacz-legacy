(in-package :klacz)

(defmacro with-page-layout (&body body)
  `(with-output-to-string (*standard-output*)
     (with-html-output (*standard-output* nil :prologue t :indent t)
       (:html
        (:head (:title "LOL URLS EVERYWHERE"))
        (:body 
         ,@body)))))

(defun print-links (links uri count offset parameters)
  (with-html-output (*standard-output* nil :indent t)
    (:table 
     (:tr (:th "User") (:th "Channel") (:th "Link") (:th "Date") (:th "Post count"))
     (loop for link in links
        do (with-slots (user channel date link post-count) link
             (htm
              (:tr (:td (fmt "~A" user))
                   (:td (fmt "~A" channel))
                   (:td (:a :href link (fmt "~A" link)))
                   (:td (fmt "~A" (format-timestring nil date :format *date-format*)))
                   (:td (fmt "~D" post-count)))))))
    (:p (when (>= offset count)
          (htm (:a :href (format nil "~A?count=~D&offset=~D~{&~(~A~)=~A~}"
                                 uri count (- offset count)
                                 parameters)
                   "Prev")))
        (:a :href (format nil "~A?count=~D&offset=~D~{&~(~A~)=~A~}" 
                          uri count (+ offset count)
                          parameters)
            "Next"))))

(defmacro define-links-page ((name uri) (&rest lambda-list) 
                             &key query (parameters nil parameters-p) sanity-checks)
  (with-gensyms (links)
    `(hunchentoot:define-easy-handler (,name :uri ,uri) 
         ((count :parameter-type 'integer :init-form 20) 
          (offset :parameter-type 'integer :init-form 0)
          ,@lambda-list)
       (unless (and count (not (minusp count))
                    offset (not (minusp offset))
                    ,@sanity-checks)
         (return-from ,name
           (with-page-layout 
             (:p "u trollin', nigga"))))
       (with-transaction 
         (let ((,links ,query))
           (with-page-layout
             (print-links ,links ,uri count offset 
                          ,(if parameters-p
                               parameters
                               `(list
                                 ,@(loop for item in lambda-list
                                    for symbol = (if (symbolp item) item (car item))
                                     append (list (intern (string symbol) :keyword)
                                                  symbol)))))))))))

(define-links-page (newest-links "/newest")
    ()
  :query (select-instances (l link)
           (order-by :descending (date-of l))
           (limit count)
           (offset offset)))

(define-links-page (user-links "/user")
    (nick)
  :query (select-instances (l link)
           (where (equal (user-of l) nick))
           (order-by :descending (date-of l))
           (limit count)
          (offset offset)))

(define-links-page (top-links "/top")
    ()
  :query (select-instances (l link)
           (order-by :descending (post-count-of l))
           (limit count)
           (offset offset)))

(define-links-page (random "/random")
    )
