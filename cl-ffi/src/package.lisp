(defpackage :ffi
  (:use :cl)
  (:export
   :use-foreign-library
   :foreign-funcall

   :allocate-foreign-value
   :free-foreign-value
   :with-foreign-value
   :with-foreign-values
   :with-foreign-memory
   :foreign-funcall))
