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

(hu.dwim.def:def hu.dwim.def:package :net.qwpx.klacz-eval 
  (:use :common-lisp :com.informatimago.common-lisp.lisp-reader.reader)
  (:shadow :*read-eval*
	   :*readtable*
	   :*read-suppress*
	   :compile
	   :compile-file
	   :compile-file-pathname
	   :defpackage
	   :directory
	   :directory-namestring
	   :do-all-symbols
	   :do-external-symbols
	   :do-symbols
	   :dribble
	   :ed
	   :enough-namestring
	   :ensure-directories-exist
	   :eval
	   :export
	   :file-author
	   :file-error
	   :file-error-pathname
	   :file-length
	   :file-namestring
	   :file-position
	   :file-stream
	   :file-string-length
	   :file-write-date
	   :find-all-symbols
	   :find-package
	   :find-symbol
	   :get-macro-character
	   :get-dispatch-macro-character
	   :import
	   :in-package
	   :intern
	   :list-all-packages
	   :load-time-value
	   :make-package
	   :make-dispatch-macro-character
	   :make-symbol
	   :merge-pathnames
	   :namestring
	   :open
	   :probe-file
	   :rename-file
	   :rename-package
	   :require
	   :readtable
	   :copy-readtable
	   :readtablep
	   :readtable-case
	   :set-syntax-from-char
	   :set-macro-character
	   :set-dispatch-macro-character
	   :shadow
	   :shadowing-import
	   :unintern
	   :unexport
	   :unuse-package
	   :user-homedir-pathname
	   :with-compilation-unit
	   :with-open-file
	   :with-package-iterator
	   :with-standard-io-syntax)
  (:shadowing-import-from :com.informatimago.common-lisp.lisp-reader.reader
			  :read
			  :read-delimited-list
			  :read-from-string
			  :read-preserving-whitespace
			  :*read-default-float-format*
			  :*read-base*)
  (:nicknames :klacz-eval))


