(in-package :klacz)

(defparameter *irc-nickname* "klacz")
(defparameter *irc-server* "irc.freenode.net")
(defparameter *irc-channels* '("#xyzzytest" "#qwpx"))

(defparameter *database-name* "klacz")
(defparameter *database-user* "klacz")
(defparameter *database-password* "klacz")

(defparameter *date-format*
  `((:day 2) #\. (:month 2) #\. (:year 4) #\space (:hour 2) #\: (:min 2) #\: (:sec 2) #\space :timezone))

(defparameter *bing-appid* "565F82ECDBD0DE7B08A4C39664E9265156F7701D")