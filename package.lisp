(defpackage :klacz
  (:use :common-lisp :cl-irc :metabang.bind :anaphora :alexandria
        :hu.dwim.perec :hu.dwim.def :local-time :cl-who)
  (:shadowing-import-from #:hu.dwim.perec #:set #:time))

(defpackage :klacz-eval 
  (:use :common-lisp))


