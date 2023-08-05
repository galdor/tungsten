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
   #:open-session
   #:close-session
   #:with-session
   #:session-host-key))
