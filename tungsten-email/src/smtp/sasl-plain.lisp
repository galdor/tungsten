(in-package :smtp)

;;; RFC 4616 The PLAIN Simple Authentication and Security Layer (SASL)
;;; Mechanism.

(defun authenticate-client/plain (client)
  (declare (type client client))
  (let ((username (client-credential client :username))
        (password (client-credential client :password)))
    (unless username
      (error 'missing-credential :mechanism :plain :name :username))
    (unless password
      (error 'missing-credential :mechanism :plain :name :password))
    (let* ((identity "")
           (credentials (format nil "~A~C~A~C~A"
                                identity
                                (code-char 0) username
                                (code-char 0) password))
           (encoded-credentials
             (text:encode-base64
              (text:encode-string credentials)))
           (message (concatenate 'string "PLAIN " encoded-credentials)))
      (write-command client "AUTH" message))
    (do-response-lines (code text client)
      (235
       nil)
      ((400 . 599)
       (error 'authentication-error :code code :text text)))))
