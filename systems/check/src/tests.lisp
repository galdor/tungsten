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

(defvar *tests* (make-hash-table :test #'equal)
  "The table containing all tests.")

(define-condition test-not-found (error)
  ((package
    :type string
    :initarg :package)
   (name
    :type string
    :initarg :name))
  (:documentation "A condition signaled when a test cannot be found.")
  (:report (lambda (condition stream)
             (with-slots (package name) condition
               (format stream "unknown test ~A"
                       (test-key package name))))))

(defun test-key (package name)
  (concatenate 'string package ":" name))

(defun register-test (test)
  "Add a test to the test table."
  (with-slots (package name) test
    (setf (gethash (test-key package name) *tests*) test)))

(defun unregister-test (test)
  "Remove a test from the test table."
  (with-slots (package name) test
    (remhash (test-key package name) *tests*)))

(defun find-test (package name)
  "Find and return a test. Signal a TEST-NOT-FOUND-CONDITION if there is no
test with this name."
  (or (gethash (test-key package name) *tests*)
      (error 'test-not-found :package package :name name)))

(defun list-tests (&key package)
  "Return all tests stored in the test table as a alist whose keys are package
names and values are list of tests.

If PACKAGE is not null, only include tests in that package."
  (let ((tests nil))
    (maphash (lambda (key test)
               (declare (ignore key))
               (unless (and package (string/= package (test-package test)))
                 (let ((pair (assoc (test-package test) tests)))
                   (if pair
                       (push test (cdr pair))
                       (push (list (test-package test) test) tests)))))
             *tests*)
    tests))
