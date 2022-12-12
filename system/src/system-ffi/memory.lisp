(in-package :system-ffi)

(defun clear-memory (%pointer size)
  (ffi:foreign-funcall "memset" ((:pointer :int size-t) :void)
                       %pointer 0 size))
