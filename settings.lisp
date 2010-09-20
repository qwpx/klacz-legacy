(in-package :klacz)

(defparameter *irc-nickname* "klacz")
(defparameter *irc-server* "irc.freenode.net")
(defparameter *irc-channels* '("#xyzzytest" "#qwpx"))

#+nil (setf iolib.sockets:*ipv6* nil)

(defparameter *database-directory* "database/")

(defparameter *date-format*
  `((:day 2) #\. (:month 2) #\. (:year 4) #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :timezone))

(defparameter *bing-appid* "565F82ECDBD0DE7B08A4C39664E9265156F7701D")

(defparameter *nickserv-password-path* (merge-pathnames "/home/dodek/programowanie/projekty/klacz/nickserv.secret"))

(defparameter *nickserv-password*
  (with-open-file (in *nickserv-password-path* :direction :input)
    (read-line in nil nil)))

(defparameter *acl*
  '(("katafrakt" . 10)
    ("x3nU" . 10)
    ("Emde" . 5)
    ("Dodecki" . 9001)
    ("stamp" . 10)
    ("mgorny" . 10)
    ("BasementCat" . 5)
    ("klausa" . 10)))


(defparameter *max-bot-lines* 4)
                                 
(defparameter *min-topic-delay* '(1 :hour))
(defparameter *min-topic-user-delay* '(2 :day))

(defparameter *topic-delimiter* "|")
