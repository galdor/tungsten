(in-package :ffi-test)

(defmacro check-foreign-value (type (&rest values)
                               &key (check 'check=))
  `(ffi:with-foreign-value (ptr ,type)
     ,@(mapcar
        (lambda (value-or-values)
          (destructuring-bind (expected-value value)
              (if (listp value-or-values)
                  value-or-values
                  (list value-or-values value-or-values))
            `(progn
               (setf (ffi:foreign-value ptr ,type) ,value)
               (,check ,expected-value (ffi:foreign-value ptr ,type)))))
        values)))

(deftest foreign-values/char ()
  (check-foreign-value :char (-128 0 127)))

(deftest foreign-values/unsigned-char ()
  (check-foreign-value :unsigned-char (0 255)))

(deftest foreign-values/short ()
  (check-foreign-value :short (-32768 0 32767)))

(deftest foreign-values/unsigned-short ()
  (check-foreign-value :unsigned-short (0 65535)))

(deftest foreign-values/int ()
  (check-foreign-value :int (-2147483648 0 2147483647)))

(deftest foreign-values/unsigned-int ()
  (check-foreign-value :unsigned-int (0 4294967295)))

(deftest foreign-values/long ()
  (ecase (ffi:foreign-type-size :long)
    (4
     (check-foreign-value :long (-2147483648 0 2147483647)))
    (8
     (check-foreign-value
      :long (-9223372036854775808 0 9223372036854775807)))))

(deftest foreign-values/unsigned-long ()
  (ecase (ffi:foreign-type-size :unsigned-long)
    (4
     (check-foreign-value :unsigned-long (4294967295)))
    (8
     (check-foreign-value :unsigned-long (18446744073709551615)))))

(deftest foreign-values/long-long ()
  (check-foreign-value
   :long-long (-9223372036854775808 0 9223372036854775807)))

(deftest foreign-values/unsigned-long-long ()
  (check-foreign-value :unsigned-long-long (0 18446744073709551615)))

(deftest foreign-values/int8 ()
  (check-foreign-value :int8 (-128 0 127)))

(deftest foreign-values/uint8 ()
  (check-foreign-value :uint8 (0 255)))

(deftest foreign-values/int16 ()
  (check-foreign-value :int16 (-32768 0 32767)))

(deftest foreign-values/uint16 ()
  (check-foreign-value :uint16 (0 65535)))

(deftest foreign-values/int32 ()
  (check-foreign-value :int32 (-2147483648 0 2147483647)))

(deftest foreign-values/uint32 ()
  (check-foreign-value :uint32 (0 4294967295)))

(deftest foreign-values/int64 ()
  (check-foreign-value
   :int64 (-9223372036854775808 0 9223372036854775807)))

(deftest foreign-values/uint64 ()
  (check-foreign-value :uint64 (0 18446744073709551615)))

(deftest foreign-values/float ()
  (check-foreign-value
   :float
   (0.0f0
    most-negative-single-float
    least-negative-single-float
    most-positive-single-float
    least-positive-single-float)))

(deftest foreign-values/double ()
  (check-foreign-value
   :double
   (0.0d0
    most-negative-double-float
    least-negative-double-float
    most-positive-double-float
    least-positive-double-float)))

(deftest foreign-values/enum ()
  (check-foreign-value
   'test-enum
   (:min
    (:min -2147483648)
    :a
    (:a 0)
    :b
    (:b 1)
    :c
    (:c 2)
    42
    :max
    (:max 2147483647))
   :check check-eql))

(deftest foreign-values/enum-with-type-alias ()
  (check-foreign-value
   'int-values
   (:a
    (:a 0)
    :b
    (:b 1)
    :c
    (:c 2))
   :check check-eql))

(deftest foreign-values/bitset ()
  (check-foreign-value
   'test-bitset
   (('() 0)
    ('(:a) 1)
    ('(:a) '(1))
    ('(:b :a) 3)
    ('(:b :a) '(1 2))
    ('(:c) 4)
    ('(:c :a) 5)
    ('(:c :b :a) 7))
   :check check-equal))

(deftest foreign-values/pointer ()
  (ffi:with-foreign-value (i :int)
    (setf (ffi:foreign-value i :int) 42)
    (ffi:with-foreign-value (ptr :pointer)
      (setf (ffi:foreign-value ptr :pointer) i)
      (check= 42 (ffi:foreign-value
                  (ffi:foreign-value ptr :pointer)
                  :int)))))

(deftest foreign-values/array ()
  (ffi:with-foreign-value (ptr :int :count 3)
    (setf (ffi:foreign-value ptr :int 0) 1
          (ffi:foreign-value ptr :int 1) 2
          (ffi:foreign-value ptr :int 2) 3)
    (check-equal '(1 2 3)
                 (list (ffi:foreign-value ptr :int 0)
                       (ffi:foreign-value ptr :int 1)
                       (ffi:foreign-value ptr :int 2)))))

(deftest foreign-strings ()
  (ffi:with-foreign-strings ((a "foobar")
                             (b "été")
                             (c ""))
    (check= 6 (ffi:foreign-string-length a))
    (check-string= "foobar" (ffi:decode-foreign-string a))
    (check-string= "foo" (ffi:decode-foreign-string a :length 3))
    (check-string= "oo" (ffi:decode-foreign-string a :offset 1 :length 2))
    (check= 5 (ffi:foreign-string-length b))
    (check-string= "été" (ffi:decode-foreign-string b))
    (check-string= "té" (ffi:decode-foreign-string b :offset 2))
    (check= 0 (ffi:foreign-string-length c))
    (check-string= "" (ffi:decode-foreign-string c))))
