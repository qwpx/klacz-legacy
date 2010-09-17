(in-package #:cl-user)

(defpackage #:klacz-system
    (:use #:cl #:asdf))

(in-package #:klacz-system)

(defsystem klacz
    :name "klacz"
    :author "Adam 'Dodek' Michalik"
    :version "0.0"
    :licence "GNU GPL v3"
    :description "Klacz IRC bot."
    :properties ((#:author-email . "dodek@dodecki.net"))
    :depends-on (:cl-irc 
                 :cl-ppcre
                 :metabang-bind 
                 :anaphora 
                 :bordeaux-threads
                 :alexandria
                 :hu.dwim.perec
                 :hu.dwim.perec.postgresql
                 :chanl
                 :ironclad
                 :drakma
                 :cl-json
                 :local-time
		 :trivial-timeout
                 :hunchentoot
                 :cl-who)
    :serial t
    :components ((:file "package")
                 (:file "settings")
                 (:file "database")
                 (:file "functions")
                 (:file "web-queries")
		 (:file "irc")
                 (:file "urlgrabber-web")))

