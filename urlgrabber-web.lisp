(in-package :klacz)

(defvar *acceptor*)

(defvar *http-output*)

(defun start-web-server ()
  (setf *acceptor* (make-instance 'hunchentoot:acceptor :port 4242))
  (hunchentoot:start *acceptor*))

(defun stop-web-server ()
  (hunchentoot:stop *acceptor*))

(defmacro with-page-layout (&body body)
  `(with-output-to-string (*http-output*)
     (with-html-output (*http-output* nil :prologue t :indent t)
       (:html
        (:head (:title "LOL URLS EVERYWHERE"))
        (:body 
         ,@body)))))

(defun print-links (links uri count offset parameters)
  (with-html-output (*http-output* nil :indent t)
    (:table 
     (:tr (:th "User") (:th "Channel") (:th "Link") (:th "Date") (:th "Post count"))
     (loop for link in links
        do (with-slots (user channel date link post-count) link
             (htm
              (:tr (:td (:a :href (format nil "/user?nick=~A" user)
                            (fmt "~A" user)))
                   (:td (:a :href (format nil "/channel?channel=~A" (remove #\# channel))
                            (fmt "~A" channel)))
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

(define-links-page (random-link "/random")
    ()
  :query (select-instances (l link)
           (limit 1)
           (offset (random (count-instances 'link)))))

(define-links-page (channel-links "/channel")
    (channel)
  :query (select-instances (l link)
           (where (equal (channel-of l) (format nil "#~A" channel)))
           (limit count)
           (offset offset)))

(define-links-page (search-links "/search")
    (q)
  :query (select-instances (l link)
	   (where (re-like (link-of l) q :case-sensitive-p nil))
	   (limit count)
	   (offset offset))
  :sanity-checks ((stringp q)))

(hunchentoot:define-easy-handler (main-page :uri "/")
    ()
  (with-page-layout 
    (with-html-output (*http-output* nil :indent t)
      (:p "I GOT SUM LINKZ:")
      (:ul
       (:li (:a :href "/newest" "HOT STUFF"))
       (:li (:a :href "/top" "TOP STUFF"))
       (:li (:a :href "/user?nick=Dodek" "MY STUFF"))
       (:li (:a :href "/channel?channel=qwpx" "OUR STUFF"))
       (:li (:a :href "/search?q=.*google.*" "GOOGLE STUFF"))
       (:li (:a :href "/random" "SO RANDOM STUFF"))))))

