(defpackage :libssh
  (:use :cl)
  (:export
   #:library-version

   #:libssh-error
   #:libssh-error-function
   #:libssh-error-code
   #:libssh-error-description

   #:open-session
   #:close-session
   #:with-session
   #:session-host-key))
