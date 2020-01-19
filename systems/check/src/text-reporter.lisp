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

(defclass text-reporter (reporter)
  ((stream
    :type stream
    :initarg :stream
    :accessor text-reporter-stream)))

(defun make-text-reporter (&key (stream *standard-output*))
  (make-instance 'text-reporter :stream stream))

(defmethod report-tests-start ((reporter text-reporter) tests)
  (with-slots (stream) reporter
    (let ((nb-tests (reduce #'+ tests :key (lambda (group)
                                             (length (cdr group))))))
      (format stream "Running ~D tests in ~D packages~%"
              nb-tests (length tests)))))

(defmethod report-tests-end ((reporter text-reporter) tests)
  (declare (ignore tests))
  (with-slots (stream nb-tests nb-test-failures) reporter
    (format stream "~%~D/~D tests passed~%"
            (- nb-tests nb-test-failures) nb-tests)))

(defmethod report-test-package-start ((reporter text-reporter) package tests)
  (declare (ignore tests))
  (with-slots (stream) reporter
    (format stream "~%Package: ~A~%" package)))

(defmethod report-test-package-end ((reporter text-reporter) package tests)
  (declare (ignore package tests))
  (with-slots (stream package-nb-tests package-nb-test-failures) reporter
    (format stream "~D/~D tests passed~%"
            (- package-nb-tests package-nb-test-failures)
            package-nb-tests)))

(defmethod report-test-start ((reporter text-reporter) test)
  (declare (ignore test))
  nil)

(defmethod report-test-success ((reporter text-reporter) test)
  (declare (ignore test))
  nil)

(defmethod report-test-failure ((reporter text-reporter) test condition)
  (with-slots (stream) reporter
    (let ((message (typecase condition
                     (test-failure
                      (test-failure-message condition))
                     (error
                      (format nil "unexpected error: ~A" condition))
                     (t
                      (format nil "unexpected condition: ~A" condition)))))
      (format stream "~A: ~A~%" (test-name test) message))))
