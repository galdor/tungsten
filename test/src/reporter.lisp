(in-package :test)

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
