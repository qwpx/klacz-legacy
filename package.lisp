(hu.dwim.def:def hu.dwim.def:package :net.qwpx.klacz
    (:nicknames :klacz)
  (:use :common-lisp :cl-irc :metabang.bind :anaphora :alexandria :ppcre
	:hu.dwim.def :hu.dwim.defclass-star :hu.dwim.perec :hu.dwim.syntax-sugar
	:local-time :closer-mop)
  (:shadowing-import-from :closer-mop
                          :defgeneric
                          :defmethod
                          :ensure-generic-function
                          :find-method
                          :remove-method
                          :standard-class
                          :standard-method
                          :standard-generic-function)
  (:shadowing-import-from :hu.dwim.perec :set :time)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-l-syntax)))

(in-package :net.qwpx.klacz)

(hu.dwim.syntax-sugar:enable-sharp-l-syntax)

(hu.dwim.def:def hu.dwim.def:package :klacz-eval 
  (:use :common-lisp))


