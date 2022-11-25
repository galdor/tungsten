(in-package :ffi)

(defmacro with-cleanup (form &body body)
  (let ((cleanup (gensym "CLEANUP-")))
    `(let ((,cleanup t))
       (unwind-protect
            (prog1 ,form
              (setf ,cleanup nil))
         (when ,cleanup
           ,@body)))))
