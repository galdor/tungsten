(in-package :test)

(defmacro check-true (expr &key label)
  (let ((value (gensym "VALUE-")))
    `(let ((,value ,expr))
       (unless ,value
         (fail "~@[~A: ~]~A yielded ~S which is not true"
               ,label ',expr ,value)))))

(defmacro check-false (expr &key label)
  (let ((value (gensym "VALUE-")))
    `(let ((,value ,expr))
       (when ,value
         (fail "~@[~A: ~]~A yielded ~S which is not false"
               ,label ',expr ,value)))))

(defmacro check-null (expr &key label)
  (let ((value (gensym "VALUE-")))
    `(let ((,value ,expr))
       (unless (null ,value)
         (fail "~@[~A: ~]~A yielded ~S which is not null"
               ,label ',expr ,value)))))

(defmacro check-is (function expected-expr value-expr &key label)
  (let ((expected (gensym "EXPECTED-"))
        (value (gensym "VALUE-")))
    `(let ((,expected ,expected-expr)
           (,value ,value-expr))
       (unless (funcall ,function ,expected ,value)
         (fail "~@[~A: ~]~S yielded ~S which is not ~A to ~S"
               ,label ',value-expr ,value ,function ,expected)))))

(defmacro check= (expected-expr value-expr &key label)
  `(check-is '= ,expected-expr ,value-expr :label ,label))

(defmacro check/= (expected-expr value-expr &key label)
  `(check-is '/= ,expected-expr ,value-expr :label ,label))

(defmacro check-eq (expected-expr value-expr &key label)
  `(check-is 'eq ,expected-expr ,value-expr :label ,label))

(defmacro check-eql (expected-expr value-expr &key label)
  `(check-is 'eql ,expected-expr ,value-expr :label ,label))

(defmacro check-equal (expected-expr value-expr &key label)
  `(check-is 'equal ,expected-expr ,value-expr :label ,label))

(defmacro check-equalp (expected-expr value-expr &key label)
  `(check-is 'equalp ,expected-expr ,value-expr :label ,label))

(defmacro check-string= (expected-expr value-expr &key label)
  `(check-is 'string= ,expected-expr ,value-expr :label ,label))

(defmacro check-string/= (expected-expr value-expr &key label)
  `(check-is 'string/= ,expected-expr ,value-expr :label ,label))

(defmacro check-string-equal (expected-expr value-expr &key label)
  `(check-is 'string-equal ,expected-expr ,value-expr :label ,label))

(defmacro check-char= (expected-expr value-expr &key label)
  `(check-is 'char= ,expected-expr ,value-expr :label ,label))

(defmacro check-char/= (expected-expr value-expr &key label)
  `(check-is 'char/= ,expected-expr ,value-expr :label ,label))

(defmacro check-signals (condition-type expr &key label)
  (let ((signaled (gensym "SIGNALED-"))
        (condition (gensym "CONDITION-")))
    `(let ((,signaled nil))
       (handler-case
           ,expr
         (,condition-type (,condition)
           (declare (ignore ,condition))
           (setf ,signaled t)))
       (unless ,signaled
         (fail "~@[~A: ~]~S did not signal a condition of type ~A"
               ,label ',expr ',condition-type)))))
