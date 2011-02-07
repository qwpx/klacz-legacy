(in-package :net.qwpx.klacz)

(setf drakma:*drakma-default-external-format* :utf-8)

(defun strip-tags (string)
  (ppcre:regex-replace-all "<.*?>" string ""))

(defun find-subtree (item tree &key (test #'equal))
  (cond 
    ((null tree) nil)
    ((atom tree) nil)
    ((funcall test item (car tree)) (cdr tree))
    (t (or (find-subtree item (car tree) :test test)
           (find-subtree item (cdr tree) :test test)))))


(defparameter *google-api-url* 
  "http://ajax.googleapis.com/ajax/services/search/web")

(defun do-google-search (term)
  (with-input-from-string (in (drakma:http-request *google-api-url* 
                                                   :parameters `(("v" . "1.0")
                                                                 ("q" . ,term))))
    (cdr (getf 
          (car (json:decode-json in)) 
          :response-data))))

(def bot-function :g (irc-reactor message line)
  (with-arglist (&rest term) (line irc-reactor message)
    (let* ((results (do-google-search term))
	  (first (first results)))
      (call-reactor irc-reactor :reply-to message 
		    (format nil "~A  -- ~A: ~A" 
			(cdr (assoc :unescaped-url first))
			(cdr (assoc :title-no-formatting first))
			(strip-tags (cdr (assoc :content first))))))))

(defparameter *google-translate-url*
  "https://www.googleapis.com/language/translate/v2")

(defun do-google-translate (term source target)
  (or
   (find-subtree 
    :translated-text
    (json:decode-json 
     (flexi-streams:make-flexi-stream 
      (flexi-streams:make-in-memory-input-stream
       (drakma:http-request *google-translate-url*
			    :parameters `(("q" . ,term)
					  ("source" . ,source)
					  ("target" . ,target)
					  ("key" . ,*google-api-key*))))
      :external-format :utf-8)))
   "Could not translate."))

(def bot-function :tr (irc-reactor message line)
  (with-arglist (source target &rest term) (line irc-reactor message)
    (call-reactor irc-reactor :reply-to message
		  (do-google-translate term source target))))

(defparameter *wolfram-api-url*
  "http://www.wolframalpha.com/input/")

(defun do-wolfram-search (term)
  (let ((result (drakma:http-request *wolfram-api-url*
                                     :parameters `(("i" . ,term))))
        all-results)
    (ppcre:do-register-groups (wolfram-result) 
        ("\\{\"stringified\": \"\\s*?(.*?)\\s*?\"" result)
      (push wolfram-result all-results))
    (mapcar #L(ppcre:regex-replace-all "\\\\n" !1 (string #\newline))
	    (nreverse all-results)))) ; dirty stuff, but who gives a shit

(def bot-function :c (irc-reactor message line)
  (with-arglist (&rest term) (line irc-reactor message)
    (let ((results (do-wolfram-search term)))
      (call-reactor irc-reactor :reply-to message 
		    (format nil "~{~A~^ | ~}" results)))))

(defparameter *bing-api-url*
  "http://api.search.live.net/json.aspx")


(defun do-bing-search (term)
  (with-input-from-string (s (babel:octets-to-string
                              (drakma:http-request *bing-api-url* 
                                                   :parameters `(("Appid" . ,*bing-appid*)
                                                                 ("query" . ,term)
                                                                 ("sources" . "web")))))
    (find-subtree :*results (json:decode-json s))))

(def bot-function :b (irc-reactor message line)
  (with-arglist (&rest term) (line irc-reactor message)
    (let ((result (first (do-bing-search term))))
      (call-reactor irc-reactor :reply-to message 
		    (format nil "~A -- ~A - ~A"
			    (cdr (assoc :*url result))
			    (cdr (assoc :*title result))
			    (cdr (assoc :*description result)))))))
