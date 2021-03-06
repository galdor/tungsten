;;; Copyright (c) 2019,2020 Nicolas Martyanoff <khaelin@gmail.com>
;;;
;;; Permission to use, copy, modify, and distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :tungsten.check)

(defmacro check-true (expr &key label)
  `(unless ,expr
     (fail "~@[~A: ~]~A is not true" ,label ',expr)))

(defmacro check-false (expr &key label)
  `(when ,expr
     (fail "~@[~A: ~]~A is not false" ,label ',expr)))

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
