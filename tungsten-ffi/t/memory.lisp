(in-package :ffi-test)

(deftest read-foreign-memory ()
  (ffi:with-foreign-value (%i :uint32)
    (setf (ffi:foreign-value %i :uint32) 67305985)
    (check-equalp #(1 2 3 4) (ffi:read-foreign-memory %i 4))))

(deftest clear-foreign-memory ()
  (ffi:with-foreign-value (%i :uint32)
    (setf (ffi:foreign-value %i :uint32) 67305985)
    (ffi:clear-foreign-memory %i 4)
    (check-equalp #(0 0 0 0) (ffi:read-foreign-memory %i 4))))
