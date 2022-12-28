(defpackage :openssl
  (:use :cl)
  (:export
   #:library-version

   #:tls-client
   #:tls-client-host
   #:tls-client-port
   #:make-tls-client))
