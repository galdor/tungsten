(defpackage :openssl
  (:use :cl)
  (:export
   #:library-version

   #:*default-ca-certificate-directory-paths*
   #:tls-client
   #:tls-client-host
   #:tls-client-port
   #:make-tls-client))
