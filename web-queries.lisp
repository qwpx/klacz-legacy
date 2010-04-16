(in-package :klacz)


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
    (with-irc 
      (reply-to message 
                (format nil "~A  -- ~A: ~A" 
                        (cdr (assoc :unescaped-url first))
                        (cdr (assoc :title-no-formatting first))
                        (cdr (assoc :content first)))))))


(defparameter *wolfram-api-url*
  "http://www.wolframalpha.com/input/")

(defun do-wolfram-search (term)
  (let ((result (drakma:http-request *wolfram-api-url*
                                     :parameters `(("i" . ,term)))))
    (ppcre:register-groups-bind (wolfram-result) 
        ("\\{\"stringified\": \"(.*?)\"" result)
     wolfram-result)))

(defbotf c (message &rest term)
  (with-irc (reply-to message (do-wolfram-search term))))