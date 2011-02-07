(in-package #:cl-user)

(defpackage #:net.qwpx.klacz-system
    (:use #:cl #:asdf))

(in-package #:net.qwpx.klacz-system)

(defsystem net.qwpx.klacz
    :name "net.qwpx.klacz"
    :author "Adam 'Dodek' Michalik"
    :version "0.0"
    :licence "GNU GPL v3"
    :description "Klacz IRC bot."
    :properties ((#:author-email . "dodek@dodecki.net"))
    :depends-on (:cl-irc :bordeaux-threads :cl-ppcre 
                 :metabang-bind :anaphora :alexandria 
		 :hu.dwim.defclass-star+hu.dwim.def :closer-mop
                 :chanl :trivial-timeout :local-time
		 :drakma :cl-json
		 :hu.dwim.perec :hu.dwim.perec.postgresql
		 :hu.dwim.syntax-sugar+hu.dwim.walker
		 :com.informatimago.common-lisp.lisp-reader)
    :serial t
    :components ((:file "package")
                 (:file "settings")
		 (:file "database")
		 (:file "utils")
		 (:file "reactor")
		 (:file "functions")
		 (:file "queries")
		 (:file "irc")
                 #+nil(:file "database")
                 #+nil(:file "functions")
                 #+nil(:file "web-queries")
		 #+nil(:file "irc")
                 #+nil(:file "urlgrabber-web")))

