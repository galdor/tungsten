(in-package :test)

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
