(in-package :postgresql-test)

(defmacro check-codec ((predicate) forms)
  `(progn
     ,@(mapcar
        (lambda (form)
          (destructuring-bind (expected-value input-value
                               &key (sql-expression "$1"))
              form
            `(let* ((rows (postgresql:query
                           (format nil "SELECT ~A" ,sql-expression)
                           (list ,input-value)))
                    (output-value (aref (car rows) 0)))
               (unless (funcall ',predicate ,expected-value output-value)
                 (fail "~S was returned as ~S which is not ~A to ~S"
                       ,input-value output-value ',predicate
                       ,expected-value)))))
        forms)))

(deftest codec/boolean ()
  (with-test-client ()
    (check-codec
     (eq)
     ((nil nil)
      (:true :true)
      (:true '(:boolean . :true))
      (:false :false)
      (:false '(:boolean . :false))))
    (check-codec
     (equalp)
     ((#(:true :false nil) '((:array :boolean) . #(:true :false nil)))))))

(deftest codec/bytea ()
  (with-test-client ()
    (check-codec
     (equalp)
     ((nil '(:bytea . nil))
      ((core:octet-vector*) (core:octet-vector*))
      ((core:octet-vector* 0 1 2 127 255) (core:octet-vector* 0 1 2 127 255))
      ((vector (core:octet-vector*)
                (core:octet-vector* 1 2 3)
               nil)
       `((:array :bytea) . #(,(core:octet-vector*)
                             ,(core:octet-vector* 1 2 3)
                             nil)))))))

(deftest codec/int2 ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:int2 . nil))
      (-32768 '(:int2 . -32768))
      (0 '(:int2 . 0))
      (32767 '(:int2 . 32767))))
    (check-codec
     (equalp)
     ((#(-1 0 42) '((:array :int2) . #(-1 0 42)))))))

(deftest codec/int4 ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:int4 . nil))
      (-2147483648 '(:int4 . -2147483648))
      (0 '(:int4 . 0))
      (2147483647 '(:int4 . 2147483647))))
    (check-codec
     (equalp)
     ((#(-1 0 42) '((:array :int4) . #(-1 0 42)))))))

(deftest codec/int8 ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:int8 . nil))
      (-9223372036854775808 '(:int8 . -9223372036854775808))
      (0 '(:int8 . 0))
      (9223372036854775807 '(:int8 . 9223372036854775807))))
    (check-codec
     (equalp)
     ((#(-1 0 42) '((:array :int8) . #(-1 0 42)))))))

(deftest codec/oid ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:oid . nil))
      (0 '(:oid . 0))
      (4294967295 '(:oid . 4294967295))))
    (check-codec
     (equalp)
     ((#(0 42 nil) '((:array :oid) . #(0 42 nil)))))))

(deftest codec/float4 ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:float4 . nil))
      (-0.5f0 '(:float4 . -0.5f0))
      (0.0f0 '(:float4 . 0.0f0))
      (3.14f0 '(:float4 . 3.14f0))))
    (check-codec
     (equalp)
     ((#(-0.5f0 0.0f0 3.14f0) '((:array :float4) . #(-0.5f0 0.0f0 3.14f0)))))))

(deftest codec/float8 ()
  (with-test-client ()
    (check-codec
     (eql)
     ((nil '(:float8 . nil))
      (-0.5d0 '(:float8 . -0.5d0))
      (0.0d0 '(:float8 . 0.0d0))
      (3.14d0 '(:float8 . 3.14d0))))
    (check-codec
     (equalp)
     ((#(-0.5d0 0.0d0 3.14d0) '((:array :float8) . #(-0.5d0 0.0d0 3.14d0)))))))

(deftest codec/text ()
  (with-test-client ()
    (check-codec
     (equal)
     ((nil '(:text . nil))
      ("" '(:text . ""))
      ("foobar" '(:text . "foobar"))
      ("été" '(:text . "été"))
      ("🙂🙃" '(:text . "🙂🙃"))))
    (check-codec
     (equalp)
     ((#("a" "bc" nil) '((:array :text) . #("a" "bc" nil)))))))

(deftest codec/timestamp ()
  (with-test-client ()
    (check-codec
     (time:datetime-equal)
     (((time:make-datetime 2023 1 31)
       (time:make-datetime 2023 1 31))
      ((time:make-datetime 2023 1 31 10 20 30 123456000)
       (time:make-datetime 2023 1 31 10 20 30 123456000))))))

(deftest codec/timestamptz ()
  (with-test-client ()
    (check-codec
     (time:datetime-equal)
     (((time:make-datetime 2023 1 31)
       `(:timestamptz . ,(time:make-datetime 2023 1 31)))
      ((time:make-datetime 2023 1 31 10 20 30 123456000)
       `(:timestamptz
         . ,(time:make-datetime 2023 1 31 10 20 30 123456000)))))))
