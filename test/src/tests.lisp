(in-package :test)

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
               (format stream "Unknown test ~S." (test-key package name))))))

(defun test-key (package name)
  (declare (type string package name))
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
  (declare (type (or string symbol) package name))
  (or (gethash (test-key (string package) (string name)) *tests*)
      (error 'test-not-found :package package :name name)))

(defun list-tests (&key package)
  "Return all tests stored in the test table as a alist whose keys are package
names and values are list of tests.

If PACKAGE is not null, only include tests in that package."
  (declare (type (or string symbol) package))
  (let ((tests nil)
        (package-name (when package (string package))))
    (maphash (lambda (key test)
               (declare (ignore key))
               (when (or (null package)
                         (string= package-name (test-package test)))
                 (let ((pair (assoc (test-package test) tests
                                    :test #'string=)))
                   (if pair
                       (push test (cdr pair))
                       (push (list (test-package test) test) tests)))))
             *tests*)
    tests))
