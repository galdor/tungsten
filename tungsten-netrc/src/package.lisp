(defpackage :netrc
  (:use :cl)
  (:export
   #:entry
   #:entry-machine
   #:entry-port
   #:entry-login
   #:entry-password
   #:entry-account
   #:search-entries*

   #:invalid-data
   #:invalid-token
   #:invalid-port-number
   #:missing-token
   #:orphaned-token
   #:parse-entries

   #:default-path
   #:load-entries*
   #:load-entries
   #:search-entries))
