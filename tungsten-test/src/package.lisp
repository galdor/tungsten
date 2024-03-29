(defpackage :test
  (:use :cl)
  (:export
   #:test-not-found
   #:deftest
   #:test
   #:test-package
   #:test-name
   #:test-function

   #:find-test
   #:list-tests

   #:test-failure
   #:test-failure-test
   #:test-failure-message
   #:fail
   #:run-test
   #:run

   #:export
   #:check-true
   #:check-false
   #:check-null
   #:check-is
   #:check=
   #:check/=
   #:check-eq
   #:check-eql
   #:check-equal
   #:check-equalp
   #:check-string=
   #:check-string/=
   #:check-string-equal
   #:check-char=
   #:check-char/=
   #:check-signals

   #:export
   #:reporter
   #:reporter-nb-tests
   #:reporter-nb-test-failures
   #:reporter-package-nb-tests
   #:reporter-package-nb-test-failures
   #:report-tests-start
   #:report-tests-end
   #:report-test-package-start
   #:report-test-package-end
   #:report-test-start
   #:report-test-success
   #:report-test-failure

   #:export
   #:text-reporter
   #:text-reporter-stream
   #:make-test-reporter

   #:export
   #:test-system
   #:test-system-and-exit))
