(in-package :klacz)

(defparameter *database-host* "127.0.0.1")
(defparameter *database-name* "klacz")
(defparameter *database-user* "klacz")
(defparameter *database-password* "klacz")




(defparameter *date-format*
  `((:day 2) #\. (:month 2) #\. (:year 4) #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :timezone))


(defparameter *klacz-path* (merge-pathnames "/home/dodek/klacz/"))
(defparameter *nickserv-password-path* (merge-pathnames "nickserv.secret"
							*klacz-path*))
(defparameter *google-api-key-path* (merge-pathnames "google.secret"
						     *klacz-path*))
(defparameter *bing-appid-path* (merge-pathnames "bing.secret"
						 *klacz-path*))

(def function read-secret (path)
  (with-open-file (in path :direction :input)
    (read-line in nil nil)))

(defparameter *nickserv-password* (read-secret *nickserv-password-path*))
(defparameter *google-api-key* (read-secret *google-api-key-path*))
(defparameter *bing-appid* (read-secret *bing-appid-path*))

(defparameter *max-bot-lines* 4)
                                 
(defparameter *min-topic-delay* '(1 :hour))
(defparameter *min-topic-user-delay* '(2 :day))

(defparameter *topic-delimiter* "|")
