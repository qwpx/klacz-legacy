(in-package :klacz)

;;; Taken from duplicates.lisp in hu.dwim.defclass-star, don't know the original 
(def function concatenate-symbol (&rest args)
  "Args are processed as parts of the result symbol with two exceptions except when a package is encountered then it is stored as the target package at intern."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                         (dolist (arg args)
                           (typecase arg
                             (string (write-string arg str))
                             (package (setf package arg))
                             (symbol (unless package
                                       (setf package (symbol-package arg)))
                                     (write-string (symbol-name arg) str))
                             (integer (write-string (princ-to-string arg) str))
                             (character (write-char arg) str)
                             (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))


(def function unzip-alist (list)
  (list (mapcar #'car list)
	(mapcar #'cdr list)))
