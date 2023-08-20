(in-package :program)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or sbcl ccl)
  (core:unsupported-feature "command line handling"))

(defun argv ()
  #+sbcl sb-ext:*posix-argv*
  #+ccl  ccl:*command-line-argument-list*)

(defun command-line-program-name ()
  (car (argv)))

(defun command-line-arguments ()
  (cdr (argv)))
