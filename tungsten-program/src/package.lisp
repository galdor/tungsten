(defpackage :program
  (:use :cl)
  (:export
   #:command-line-program-name
   #:command-line-arguments

   #:*programs*
   #:*program*
   #:program
   #:program-name
   #:program-function
   #:unknown-program
   #:unknown-program-name
   #:defprogram
   #:build-executable
   #:option-value
   #:argument-value))
