(defpackage :ffi
  (:use :cl)
  (:export
   :use-foreign-library
   :use-asdf-shared-library

   :allocate-foreign-value
   :free-foreign-value
   :with-foreign-value
   :with-foreign-values
   :with-foreign-memory
   :foreign-value-ref

   :foreign-funcall))
