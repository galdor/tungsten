(in-package :ffi-test)

(ffi:use-asdf-shared-library 'ffi-test "ffi/test" "ffi-test")

(ffi:define-enum (test-enum)
    ((:test-enum-min -2147483648)
     (:test-enum-0 0)
     (:test-enum-1 1)
     (:test-enum-2 2)
     (:test-enum-max 2147483647)))

(ffi:define-struct (test-struct-packed)
    ((:a :int8)
     (:b :int8)
     (:c :int8)))

(ffi:define-struct (test-struct-padding)
    ((:a :int64)
     (:b :int8)
     (:c :int16)))

(ffi:define-struct (test-struct-arrays)
    ((:a :int8 :count 2)
     (:b :float :count 3)))

(ffi:define-struct (test-struct-nested)
    ((:s 'test-struct-packed)
     (:s2 'test-struct-padding :count 2)
     (:ps :pointer)))
