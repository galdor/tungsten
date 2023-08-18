(in-package :program)

(define-condition unknown-program (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-program-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown program ~A." name)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun program-main-function (name)
    (declare (type symbol name))
    (intern (concatenate 'string "MAIN-" (symbol-name name)))))

(defmacro define-program ((name) &body body)
  `(defun ,(program-main-function name) ()
     (let ((core:*interactive* nil))
       ,@body)))

(defun build-executable (program-name &key path)
  (declare (type symbol program-name)
           (type (or pathname string null) path))
  (let ((path (or path (string-downcase (symbol-name program-name))))
        (main (program-main-function program-name)))
    (unless (fboundp main)
      (error 'unknown-program :name program-name))
    #+sbcl
    (let ((args (append
                 (list :executable t)
                 (list :toplevel main)
                 (when (member :sb-core-compression *features*)
                   (list :compression t)))))
      (apply 'sb-ext:save-lisp-and-die path args))
    #+ccl
    (ccl:save-application path :prepend-kernel t
                               :purify t
                               :toplevel-function main)
    #-(or sbcl ccl)
    (core:unsupported-feature "executable creation")))
