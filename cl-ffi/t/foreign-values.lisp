(in-package :ffi-test)

(defmacro check-foreign-value-ref (type &rest values)
  `(ffi:with-foreign-value (ptr ,type)
     ,@(mapcar (lambda (value)
                 `(progn
                    (setf (ffi:foreign-value-ref ptr ,type) ,value)
                    (check= ,value (ffi:foreign-value-ref ptr ,type))))
               values)))

(deftest foreign-values/char ()
  (check-foreign-value-ref :char -128 0 127))

(deftest foreign-values/unsigned-char ()
  (check-foreign-value-ref :unsigned-char 0 255))

(deftest foreign-values/short ()
  (check-foreign-value-ref :short -32768 0 32767))

(deftest foreign-values/unsigned-short ()
  (check-foreign-value-ref :unsigned-short 0 65535))

(deftest foreign-values/int ()
  (check-foreign-value-ref :int -2147483648 0 2147483647))

(deftest foreign-values/unsigned-int ()
  (check-foreign-value-ref :unsigned-int 0 4294967295))

(deftest foreign-values/long ()
  (ecase (ffi:foreign-type-size :long)
    (4
     (check-foreign-value-ref :long -2147483648 0 2147483647))
    (8
     (check-foreign-value-ref
      :long -9223372036854775808 0 9223372036854775807))))

(deftest foreign-values/unsigned-long ()
  (ecase (ffi:foreign-type-size :unsigned-long)
    (4
     (check-foreign-value-ref :unsigned-long 4294967295))
    (8
     (check-foreign-value-ref :unsigned-long 18446744073709551615))))

(deftest foreign-values/long-long ()
  (check-foreign-value-ref
   :long-long -9223372036854775808 0 9223372036854775807))

(deftest foreign-values/unsigned-long-long ()
  (check-foreign-value-ref :unsigned-long-long 0 18446744073709551615))

(deftest foreign-values/int8 ()
  (check-foreign-value-ref :int8 -128 0 127))

(deftest foreign-values/uint8 ()
  (check-foreign-value-ref :uint8 0 255))

(deftest foreign-values/int16 ()
  (check-foreign-value-ref :int16 -32768 0 32767))

(deftest foreign-values/uint16 ()
  (check-foreign-value-ref :uint16 0 65535))

(deftest foreign-values/int32 ()
  (check-foreign-value-ref :int32 -2147483648 0 2147483647))

(deftest foreign-values/uint32 ()
  (check-foreign-value-ref :uint32 0 4294967295))

(deftest foreign-values/int64 ()
  (check-foreign-value-ref :int64 -9223372036854775808 0 9223372036854775807))

(deftest foreign-values/uint64 ()
  (check-foreign-value-ref :uint64 0 18446744073709551615))

(deftest foreign-values/float ()
  (check-foreign-value-ref :float
                           0.0
                           most-negative-single-float
                           least-negative-single-float
                           most-positive-single-float
                           least-positive-single-float))

(deftest foreign-values/double ()
  (check-foreign-value-ref :double
                           0.0d0
                           most-negative-double-float
                           least-negative-double-float
                           most-positive-double-float
                           least-positive-double-float))

(deftest foreign-values/pointer ()
  (ffi:with-foreign-value (i :int)
    (setf (ffi:foreign-value-ref i :int) 42)
    (ffi:with-foreign-value (ptr (:pointer :int))
      (setf (ffi:foreign-value-ref ptr :pointer) i)
      (check= 42 (ffi:foreign-value-ref
                  (ffi:foreign-value-ref ptr :pointer)
                  :int)))))

(deftest foreign-values/array ()
  (ffi:with-foreign-value (ptr (:array :int 3))
    (setf (ffi:foreign-value-ref ptr :int 0) 1
          (ffi:foreign-value-ref ptr :int 1) 2
          (ffi:foreign-value-ref ptr :int 2) 3)
    (check-equal '(1 2 3)
                 (list (ffi:foreign-value-ref ptr :int 0)
                       (ffi:foreign-value-ref ptr :int 1)
                       (ffi:foreign-value-ref ptr :int 2)))))
