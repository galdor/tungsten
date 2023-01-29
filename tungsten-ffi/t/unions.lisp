(in-package :ffi-test)

(deftest unions ()
  (ffi:with-foreign-value (%pointer 'test-union)
    (ffi:with-foreign-string (%string "foobar")
      (setf (ffi:foreign-union-member %pointer 'test-union :a) %string)
      (check-string= "foobar"
                     (ffi:decode-foreign-string
                      (ffi:foreign-union-member %pointer 'test-union :a))))
    (setf (ffi:foreign-union-member %pointer 'test-union :b) 42)
    (check= 42 (ffi:foreign-union-member %pointer 'test-union :b))
    (setf (ffi:foreign-union-member %pointer 'test-union :c 0) -2147483648
          (ffi:foreign-union-member %pointer 'test-union :c 1) 0
          (ffi:foreign-union-member %pointer 'test-union :c 2) 42
          (ffi:foreign-union-member %pointer 'test-union :c 3) 2147483647)
    (check= -2147483648 (ffi:foreign-union-member %pointer 'test-union :c 0))
    (check= 0 (ffi:foreign-union-member %pointer 'test-union :c 1))
    (check= 42 (ffi:foreign-union-member %pointer 'test-union :c 2))
    (check= 2147483647 (ffi:foreign-union-member %pointer 'test-union :c 3))))
