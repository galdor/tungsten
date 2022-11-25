(in-package :ffi-test)

(ffi:use-asdf-shared-library 'ffi-test "ffi/test" "ffi-test")

(deftest foreign-funcall/void ()
  (ffi:foreign-funcall "ffi_test_void" (() :void)))

(deftest foreign-funcall/int ()
  (check= 42 (ffi:foreign-funcall "ffi_test_add2_int"
                                  ((:int :int) :int) 40 2)))

(deftest foreign-funcall/int-pointer ()
  (ffi:with-foreign-value (ptr :int)
    (ffi:foreign-funcall "ffi_test_add2_int_ptr"
                         ((:int :int :pointer) :void) 40 2 ptr)
    (check= 42 (ffi:foreign-value-ref ptr :int))))
