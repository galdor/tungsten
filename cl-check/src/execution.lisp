(in-package :check)

(defvar *test* nil
  "The test being currently executed.")

(define-condition test-failure (error)
  ((test
    :type (or test null)
    :initarg :test
    :accessor test-failure-test)
   (message
    :type string
    :initarg :message
    :accessor test-failure-message))
  (:documentation "A condition indicating that a test failed. If TEST is null,
  then the condition was signaled out of a test; it may seems strange to
  support it, but it makes it a lot easier to test assertion macros
  interactively.")
  (:report (lambda (condition stream)
             (with-slots (test message) condition
               (if test
                   (format stream "test ~A failed: ~A"
                           (test-key (test-package test) (test-name test))
                           message)
                   (princ message stream))))))

(defun fail (format &rest args)
  "Signal a test failure error."
  (error 'test-failure :test *test*
                       :message (format nil "~?" format args)))

(defun run-test (test)
  "Run a test."
  (let ((*test* test))
    (funcall (test-function test))))

(defun run (&key package (reporter (make-text-reporter)))
  "Run all registered tests, optionally filtered by package. Return T if all
tests passed or NIL else.

If REPORTER is not null, it is used to report test execution. For the purpose
of reporting, tests are grouped by package. The default reporter is
TEXT-REPORTER."
  (flet ((report (function &rest args)
           (when reporter
             (apply function reporter args))))
    (let ((tests (list-tests :package package))
          (success t))
      (report 'report-tests-start tests)
      (restart-case
          (dolist (group tests)
            (let ((package (car group))
                  (group-tests (cdr group)))
              (report 'report-test-package-start package group-tests)
              (dolist (test group-tests)
                (restart-case
                    (progn
                      (report 'report-test-start test)
                      (handler-bind
                          ((condition
                             (lambda (condition)
                               (report 'report-test-failure test condition)
                               (setf success nil))))
                        (run-test test)
                        (report 'report-test-success test)))
                  (continue ()
                    :report "Abort the current test and continue execution."
                    nil)))
              (report 'report-test-package-end package group-tests)))
        (stop ()
          :report "Stop test execution."
          nil))
      (report 'report-tests-end tests)
      success)))
