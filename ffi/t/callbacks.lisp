(in-package :ffi-test)

(ffi:defcallback (test-call-fn ((:pointer :int) :int) %string n)
  (let ((string (ffi:decode-foreign-string %string)))
    (* (length string) n)))

(ffi:defcallback (test-call-enum-in-fn ((test-enum) :unsigned-int) value)
  (length (symbol-name value)))

(deftest callbacks ()
  (check= 462
          (ffi:with-foreign-string (%string "hello world")
            (ffi:foreign-funcall "ffi_test_call"
                                 ((:pointer :pointer :int) :int)
                                 (ffi:callback-pointer 'test-call-fn)
                                 %string 42)))
  (check= 3
          (ffi:foreign-funcall "ffi_test_call_enum_in"
                               ((:pointer test-enum) :unsigned-int)
                               (ffi:callback-pointer 'test-call-enum-in-fn)
                               :max)))
