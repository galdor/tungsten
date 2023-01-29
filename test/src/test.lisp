(in-package :test)

(defmacro deftest (name () &body body)
  "Define a test."
  (let ((function (gensym "FUNCTION-"))
        (test (gensym "TEST-")))
    `(let* ((,function (lambda () (progn ,@body nil)))
            (,test (make-instance 'test :package (package-name *package*)
                                        :name (symbol-name ',name)
                                        :function ,function)))
       (register-test ,test)
       (test-name ,test))))

(defclass test ()
  ((package
    :type string
    :initarg :package
    :reader test-package)
   (name
    :type string
    :initarg :name
    :reader test-name)
   (function
    :type function
    :initarg :function
    :accessor test-function)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t)
    (format stream "~A" (test-name test))))
