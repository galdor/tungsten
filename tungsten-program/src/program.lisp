(in-package :program)

(defvar *programs* (make-hash-table))

(defvar *program* nil)

(defclass program ()
  ((name
    :type symbol
    :initarg :name
    :reader program-name)
   (function
    :type function
    :reader program-function)))

(define-condition unknown-program (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-program-name))
  (:report
   (lambda (condition stream)
     (format stream "unknown program ~A" (unknown-program-name condition)))))

(defmacro defprogram ((name) &body body)
  (let ((function (gensym "FUNCTION-"))
        (program (gensym "PROGRAM-")))
    `(let* ((,program (make-instance 'program :name ',name))
            (,function
              (lambda ()
                (let ((core:*interactive* nil)
                      (*program* ,program))
                  ,@body))))
       (setf (slot-value ,program 'function) ,function)
       (setf (gethash ',name *programs*) ,program))))

(defun program (name)
  (or (gethash name *programs*)
      (error 'unknown-program :name 'name)))

(defun build-executable (program-name &key path)
  (declare (type symbol program-name)
           (type (or pathname string null) path))
  (let ((path (or path (string-downcase (symbol-name program-name))))
        (program (program program-name)))
    #+sbcl
    (let ((args (append
                 (list :executable t)
                 (list :toplevel (program-function program))
                 (when (member :sb-core-compression *features*)
                   (list :compression t)))))
      (apply 'sb-ext:save-lisp-and-die path args))
    #+ccl
    (ccl:save-application path :prepend-kernel t
                               :purify t
                               :toplevel-function (program-function program))
    #-(or sbcl ccl)
    (core:unsupported-feature "executable creation")))
