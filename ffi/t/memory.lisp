(in-package :ffi-test)

(deftest read-foreign-memory ()
  (ffi:with-foreign-value (%i :uint32)
    (setf (ffi:foreign-value %i :uint32) 67305985)
    (check-equalp #(1 2 3 4) (ffi:read-foreign-memory %i 4))))
