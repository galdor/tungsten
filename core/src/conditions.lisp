(in-package :core)

(define-condition unsupported-feature ()
  ((name
    :type string
    :initarg :name
    :reader unsupported-feature-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "~@(~A~) is not available on the current platform."
               name)))))

(defun unsupported-feature (name)
  (error 'unsupported-feature :name name))

(defmacro abort-protect (form &body cleaning-forms)
  "Evaluate FORM. If the execution of FORM does not complete, evaluate
CLEANING-FORMS."
  (let ((aborted (gensym "ABORTED-")))
    `(let ((,aborted t))
       (unwind-protect
            (multiple-value-prog1
                ,form
              (setf ,aborted nil))
         (when ,aborted
           ,@cleaning-forms)))))
