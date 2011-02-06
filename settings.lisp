(in-package :klacz)


(defparameter *date-format*
  `((:day 2) #\. (:month 2) #\. (:year 4) #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :timezone))

(defparameter *bing-appid* "565F82ECDBD0DE7B08A4C39664E9265156F7701D")

(defparameter *nickserv-password-path* (merge-pathnames "/home/dodek/programowanie/projekty/klacz/nickserv.secret"))

(defparameter *nickserv-password*
  (with-open-file (in *nickserv-password-path* :direction :input)
    (read-line in nil nil)))


(defparameter *max-bot-lines* 4)
                                 
