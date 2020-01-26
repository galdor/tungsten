;;; Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>
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

(in-package :tungsten.system)

(define-condition foreign-error (error)
  ((function
    :type (or symbol function)
    :initarg :function
    :accessor foreign-error-function
    :documentation "A designator of the foreign function which triggered the
    error.")
   (value
    :type (or null integer keyword)
    :initarg :value
    :initform nil
    :accessor foreign-error-value
    :documentation "An error value, keyword or integer, identifying the cause
of the error. Usually obtained from errno.")
   (description
    :type string
    :initarg :description
    :accessor foreign-error-description))
  (:documentation "A condition representing an error returned by a foreign
function.")
  (:report (lambda (condition stream)
             (with-slots (function value description) condition
               (format stream "~A failed~@[ with error ~A~]: ~A"
                       function value description)))))

(defmacro define-errno-error (name errno)
  "Define a condition inheriting from FOREIGN-ERROR representing errors
associated with ERRNO."
  `(progn
     (define-condition ,name (foreign-error)
       ()
       (:documentation
        ,(format nil (concatenate 'string
                                  "A condition representing a foreign error"
                                  " identified by the errno value ~A.")
                 (symbol-name errno))))
     (defmethod initialize-instance :after ((error ,name) &key)
       (with-slots (value description) error
         (setf value ,errno)
         (setf description (errno-description ,errno))))))

(define-errno-error interrupted-system-call :eintr)
