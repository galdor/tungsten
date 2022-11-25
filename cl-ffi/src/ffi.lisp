(in-package :ffi)

;;;
;;; Memory
;;;

(defmacro with-foreign-value ((ptr-var type &key (count 1)) &body body)
  (if (and (constantp type) (constantp count))
      `(%with-foreign-value (,ptr-var ,type :count ,count)
         ,@body)
      `(let ((,ptr-var (%allocate-foreign-memory
                        (* (%foreign-type-size ,type) ,count))))
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

(defmacro foreign-value-ref (ptr type &optional (offset 0))
  (if (constantp offset)
      `(,(%foreign-type-ref-function type)
        ,ptr
        ,(* (%foreign-type-size type) offset))
      `(,(%foreign-type-ref-function type)
        ,ptr
        (* (%foreign-type-size ,type) ,offset))))

;;;
;;; Foreign calls
;;;

(defmacro foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  `(%foreign-funcall ,name ((,@arg-types) ,return-type) ,@args))
