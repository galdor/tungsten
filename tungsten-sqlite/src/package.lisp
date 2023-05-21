(defpackage :sqlite
  (:use :cl)
  (:export
   #:library-version

   #:sqlite-error
   #:sqlite-error-function
   #:sqlite-error-code
   #:sqlite-error-description))
