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
    :depends-on (:cl-irc :bordeaux-threads :drakma :hunchentoot
                 :cl-ppcre :cl-json
                 :metabang-bind :anaphora :alexandria 
		 :hu.dwim.defclass-star+hu.dwim.def :closer-mop
                 :chanl :trivial-timeout
                 :ironclad :cl-who :local-time 
                 :cl-prevalence)
    :serial t
    :components ((:file "package")
                 (:file "settings")
		 (:file "utils")
		 (:file "reactor")
		 (:file "irc")
                 (:file "database")
                 #+nil(:file "functions")
                 #+nil(:file "web-queries")
		 #+nil(:file "irc")
                 #+nil(:file "urlgrabber-web")))

