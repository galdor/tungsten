(in-package :core)

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
