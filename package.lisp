(defpackage :klacz
  (:use :common-lisp :cl-irc :metabang.bind :anaphora :alexandria
        :hu.dwim.perec :hu.dwim.def :local-time)
  (:shadowing-import-from #:hu.dwim.perec #:set #:time))

