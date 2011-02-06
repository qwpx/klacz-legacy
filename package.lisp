(defpackage :net.qwpx.klacz
  (:nicknames :klacz)
  (:use :common-lisp :cl-irc :metabang.bind :anaphora :alexandria :ppcre
	:hu.dwim.def :hu.dwim.defclass-star 
	:local-time :closer-mop)
  (:shadowing-import-from :closer-mop
                          :defgeneric
                          :defmethod
                          :ensure-generic-function
                          :find-method
                          :remove-method
                          :standard-class
                          :standard-method
                          :standard-generic-function))

(defpackage :klacz-eval 
  (:use :common-lisp))


