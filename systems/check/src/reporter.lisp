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

(defclass reporter ()
  ((nb-tests
    :type (integer 0)
    :initform 0
    :accessor reporter-nb-tests)
   (nb-test-failures
    :type (integer 0)
    :initform 0
    :accessor reporter-nb-test-failures)
   (package-nb-tests
    :type (or null (integer 0))
    :initform nil
    :accessor reporter-package-nb-tests)
   (package-nb-test-failures
    :type (or null (integer 0))
    :initform nil
    :accessor reporter-package-nb-test-failures)))

(defgeneric report-tests-start (reporter tests))

(defgeneric report-tests-end (reporter tests))

(defgeneric report-test-package-start (reporter package tests)
  (:method :before ((reporter reporter) package tests)
    (declare (ignore package tests))
    (with-slots (package-nb-tests package-nb-test-failures) reporter
      (setf package-nb-tests 0)
      (setf package-nb-test-failures 0))))

(defgeneric report-test-package-end (reporter package tests)
  (:method :after ((reporter reporter) package tests)
    (declare (ignore package tests))
    (with-slots (package-nb-tests package-nb-test-failures) reporter
      (setf package-nb-tests nil)
      (setf package-nb-test-failures nil))))

(defgeneric report-test-start (reporter test)
  (:method :before ((reporter reporter) test)
    (declare (ignore test))
    (with-slots (nb-tests package-nb-tests) reporter
      (incf nb-tests)
      (incf package-nb-tests))))

(defgeneric report-test-success (reporter test))

(defgeneric report-test-failure (reporter test condition)
  (:method :after ((reporter reporter) test condition)
    (declare (ignore test condition))
    (with-slots (nb-test-failures package-nb-test-failures) reporter
      (incf nb-test-failures)
      (incf package-nb-test-failures))))
