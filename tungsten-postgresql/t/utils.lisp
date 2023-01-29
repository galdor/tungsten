(in-package :postgresql-test)

;; See docker-compose.yaml
(defparameter *test-username* "postgres")
(defparameter *test-password* "postgres")

(defmacro with-test-client (() &body body)
  `(postgresql:with-client (:user *test-username* :password *test-password*
                            :application-name "tungsten")
     ,@body))
