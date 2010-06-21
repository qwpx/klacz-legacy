(in-package :klacz)

(setf drakma:*drakma-default-external-format* :utf-8)

(defparameter *google-api-url* 
  "http://ajax.googleapis.com/ajax/services/search/web")

(defun do-google-search (term)
  (with-input-from-string (in (drakma:http-request *google-api-url* 
                                                   :parameters `(("v" . "1.0")
                                                                 ("q" . ,term))))
    (cdr (getf 
          (car (json:decode-json in)) 
          :response-data))))


(defbotf g (message &rest term)
  (bind ((results (do-google-search term))
         (first (first results)))
    (within-irc 
      (reply-to message 
                (format nil "~A  -- ~A: ~A" 
                        (cdr (assoc :unescaped-url first))
                        (cdr (assoc :title-no-formatting first))
                        (cdr (assoc :content first)))))))


(defparameter *wolfram-api-url*
  "http://www.wolframalpha.com/input/")

(defun do-wolfram-search (term)
  (let ((result (drakma:http-request *wolfram-api-url*
                                     :parameters `(("i" . ,term))))
        all-results)
    (ppcre:do-register-groups (wolfram-result) 
        ("\\{\"stringified\": \"\\s*?(.*?)\\s*?\"" result)
      (push wolfram-result all-results))
    (nreverse all-results))) ; dirty stuff, but who gives a shit



(defbotf c (message &rest term)
  (within-irc (reply-to message 
              (format nil "~{~A~^ | ~}"
                      (do-wolfram-search term)))))

(defparameter *bing-api-url*
  "http://api.search.live.net/json.aspx")

(defun find-subtree (item tree &key (test #'equal))
  (cond 
    ((null tree) nil)
    ((atom tree) nil)
    ((funcall test item (car tree)) (cdr tree))
    (t (or (find-subtree item (car tree) :test test)
           (find-subtree item (cdr tree) :test test)))))

(defun do-bing-search (term)
  (with-input-from-string (s (babel:octets-to-string
                              (drakma:http-request *bing-api-url* 
                                                   :parameters `(("Appid" . ,*bing-appid*)
                                                                 ("query" . ,term)
                                                                 ("sources" . "web")))))
    (find-subtree :*results (json:decode-json s))))

(defbotf b (message &rest term)
  (bind ((result (first (do-bing-search term))))
    (within-irc 
      (reply-to message 
                (format nil "~A -- ~A - ~A"
                        (cdr (assoc :*url result))
                        (cdr (assoc :*title result))
                        (cdr (assoc :*description result)))))))
