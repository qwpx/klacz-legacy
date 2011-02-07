(in-package :com.informatimago.common-lisp.lisp-reader.reader)


(defparser parse-symbol-token (token)
  (let ((colon (position-if
                (lambda (traits) (traitp +ct-package-marker+ traits))
                (token-traits token))))
    (when colon
      (let* ((double-colon (and (< (1+ colon) (token-length token))
				(traitp +ct-package-marker+
					(token-char-traits token (1+ colon)))))
	     (pname (subseq (token-text token) 0 colon))
	     (sname (subseq (token-text token)
			    (+ colon (if double-colon 2 1)))))
	  (when (zerop colon)
            ;; Keywords always exist, so let's intern them before finding them.
            (setf pname "KEYWORD")
            (accept 'symbol (intern sname pname)))))
	  ;; no colon in token, let's just intern the symbol in the current package :
    (accept 'symbol (intern (token-text token) *package*))))

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

(def function map-plist (function plist)
  (loop 
     for (key value . rest) on plist by #'cddr
     collect (funcall function key value)))

(def function mapc-plist (function plist)
  (loop 
     for (key value . rest) on plist by #'cddr
     do (funcall function key value)))
