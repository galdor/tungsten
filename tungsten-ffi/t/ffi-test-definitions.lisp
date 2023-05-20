(in-package :ffi-test)

(ffi:use-asdf-shared-library 'ffi-test "tungsten-ffi/test" "ffi-test"
                             :reload t)

(ffi:define-type-alias ffi-int-t :int)

(ffi:define-foreign-enumeration (int-values :base-type 'ffi-int-t)
    ((:a 0)
     (:b 1)
     (:c 2)))

(ffi:define-foreign-enumeration (test-enum)
    ((:min -2147483648)
     (:a             0)
     (:b             1)
     (:c             2)
     (:max  2147483647)))

(ffi:define-foreign-bitset (test-bitset)
    ((:a    #x00000001)
     (:b    #x00000002)
     (:c    #x00000004)))

(ffi:define-foreign-structure (test-struct-packed)
    ((:a :int8)
     (:b :int8)
     (:c :int8)))

(ffi:define-foreign-structure (test-struct-padding)
    ((:a :int64)
     (:b :int8)
     (:c :int16)))

(ffi:define-foreign-structure (test-struct-arrays)
    ((:a :int8 :count 2)
     (:b :float :count 3)))

(ffi:define-foreign-structure (test-struct-nested)
    ((:s 'test-struct-packed)
     (:s2 'test-struct-padding :count 2)
     (:ps :pointer)))

(ffi:define-foreign-union (test-union)
  ((:a :pointer)
   (:b :uint8)
   (:c :int32 :count 4)))
