(in-package :ffi-test)

(deftest foreign-funcall/void ()
  (ffi:foreign-funcall "ffi_test_void" (() :void)))

(deftest foreign-funcall/int ()
  (check= 42 (ffi:foreign-funcall "ffi_test_add2_int"
                                  ((:int :int) :int) 40 2)))

(deftest foreign-funcall/int-pointer ()
  (ffi:with-foreign-value (ptr :int)
    (ffi:foreign-funcall "ffi_test_add2_int_ptr"
                         ((:int :int :pointer) :void) 40 2 ptr)
    (check= 42 (ffi:foreign-value ptr :int))))

(deftest foreign-funcall/strings ()
  (ffi:with-foreign-strings ((a "foo")
                             (b "bar"))
    (let ((ab (ffi:foreign-funcall "ffi_test_concat"
                                   ((:pointer :pointer) :pointer) a b)))
      (unwind-protect
           (check-string= "foobar" (ffi:decode-foreign-string ab))
        (ffi:free-foreign-memory ab)))))
