(defpackage :libssh
  (:use :cl)
  (:export
   #:library-version

   #:libssh-error
   #:libssh-error-function
   #:libssh-error-code
   #:libssh-error-description

   #:server-authentication-error
   #:server-authentication-error-host-key
   #:unknown-host-key
   #:host-key-mismatch
   #:host-key-type-mismatch
   #:client-authentication-error
   #:public-key-authentication-not-supported
   #:public-key-authentication-failure

   #:*client-read-block-size*
   #:client
   #:make-client
   #:disconnect-client
   #:with-client
   #:execute))
