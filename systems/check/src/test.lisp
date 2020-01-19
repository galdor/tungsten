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

(defmacro deftest (name () &body body)
  "Define a test."
  (let ((function (gensym "FUNCTION-"))
        (test (gensym "TEST-")))
    `(let* ((,function (lambda () (progn ,@body nil)))
            (,test (make-instance 'test :package (package-name *package*)
                                        :name (symbol-name ',name)
                                        :function ,function)))
       (register-test ,test)
       (test-name ,test))))

(defclass test ()
  ((package
    :type string
    :initarg :package
    :reader test-package)
   (name
    :type string
    :initarg :name
    :reader test-name)
   (function
    :type function
    :initarg :function
    :accessor test-function)))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t)
    (format stream "~A" (test-name test))))
