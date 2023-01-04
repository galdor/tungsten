(defpackage :openssl
  (:use :cl)
  (:export
   #:library-version
   #:openssl-error
   #:openssl-error-value
   #:openssl-error-reason
   #:openssl-error-description
   #:openssl-error-file
   #:openssl-error-line
   #:openssl-error-function
   #:openssl-error-data
   #:openssl-error-flags
   #:openssl-error-stack
   #:openssl-error-stack-function
   #:openssl-error-stack-errors

   #:*default-ca-certificate-directory-paths*
   #:tls-client
   #:tls-client-host
   #:tls-client-port
   #:make-tls-client))
