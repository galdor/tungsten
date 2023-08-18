(defpackage :program
  (:use :cl)
  (:export
   #:program-name
   #:command-line-arguments

   #:unknown-program
   #:unknown-program-name
   #:define-program
   #:build-executable))
