(in-package :ffi)

;;;
;;; Memory
;;;

(defmacro allocate-foreign-value (type)
  `(%allocate-foreign-value ,type))

(defmacro free-foreign-value (ptr type)
  `(%free-foreign-value ,ptr ,type))

(defmacro with-foreign-value ((ptr-var type) &body body)
  (if (or (constantp type)
          (and (listp type) (every #'constantp type)))
      `(%with-foreign-value (,ptr-var ,type)
         ,@body)
      `(let ((,ptr-var (%allocate-foreign-memory (%foreign-type-size ',type))))
         (unwind-protect
              (progn ,@body)
           (%free-foreign-memory ,ptr-var)))))

(defmacro with-foreign-values ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-value ,(car bindings)
         (with-foreign-values ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

;;;
;;; Foreign calls
;;;

(defmacro foreign-funcall (name (&rest arg-types) return-type &rest args)
  `(%foreign-funcall ,name (,@arg-types) ,return-type ,@args))
