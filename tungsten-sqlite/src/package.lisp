(defpackage :sqlite
  (:use :cl)
  (:export
   #:sqlite-error
   #:sqlite-error-function
   #:sqlite-error-code
   #:sqlite-error-description

   #:library-version))
