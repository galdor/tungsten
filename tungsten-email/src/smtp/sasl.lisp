(in-package :smtp)

(defgeneric authenticate (mechanism client challenge))

(define-condition missing-credential ()
  ((mechanism
    :type symbol
    :initarg :mechanism
    :reader missing-credential-mechanism)
   (name
    :type symbol
    :initarg :name
    :reader missing-credential-name)))

(define-condition authentication-error (error)
  ((code
    :type integer
    :initarg :code
    :reader authentication-error-code)
   (text
    :type string
    :initarg :text
    :reader authentication-error-text))
  (:report
   (lambda (condition stream)
     (format stream "SMTP authentication error (code ~D): ~A"
             (authentication-error-code condition)
             (authentication-error-text condition)))))

(defun authenticate-client (client)
  (declare (type client client))
  (with-slots (keywords tls) client
    (let ((supported-mechanisms (cdr (assoc :auth keywords))))
      (dolist (mechanism supported-mechanisms)
        (handler-case
            (cond
              ((and (eq mechanism :plain) tls)
               (authenticate-client/plain client)))
          (missing-credential ()
            nil))))))
