(in-package :ffi-test)

(ffi:use-asdf-shared-library 'ffi-test "ffi/test" "ffi-test"
                             :reload t)

(ffi:define-enum (test-enum)
    ((:min -2147483648)
     (:a             0)
     (:b             1)
     (:c             2)
     (:max  2147483647)))

(ffi:define-bitset (test-bitset)
    ((:a    #x00000001)
     (:b    #x00000002)
     (:c    #x00000004)))

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

(ffi:define-foreign-union (test-union)
  ((:a :pointer)
   (:b :uint8)
   (:c :int32 :count 4)))
