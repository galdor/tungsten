(in-package :ffi-test)

(deftest errno ()
  (ffi:with-foreign-strings ((path "")
                             (mode "r"))
    (ffi:foreign-funcall "fopen" ((:pointer :pointer) :pointer) path mode))
  (check/= (ffi:errno) 0))
