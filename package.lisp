(defpackage :klacz
  (:use :common-lisp :cl-irc :metabang.bind :anaphora :alexandria
	:hu.dwim.def :hu.dwim.defclass-star 
	:local-time :cl-who :cl-prevalence :closer-mop)
  (:shadowing-import-from :cl-prevalence :name)
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


