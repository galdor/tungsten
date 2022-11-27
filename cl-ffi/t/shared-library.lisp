(in-package :ffi-test)

(ffi:use-asdf-shared-library 'ffi-test "ffi/test" "ffi-test")

(ffi:define-enumeration (test-enum)
  ((:test-enum-min -2147483648)
   (:test-enum-0 0)
   (:test-enum-1 1)
   (:test-enum-2 2)
   (:test-enum-max 2147483647)))
