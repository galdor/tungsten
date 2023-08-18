(in-package :program)

(defun argv ()
  #+sbcl
  sb-ext:*posix-argv*
  #+ccl
  ccl:*command-line-argument-list*
  #-(or sbcl ccl)
  (core:unsupported-feature "command line argument handling"))

(defun program-name ()
  #+(or sbcl ccl)
  (car (argv))
  #-(or sbcl ccl)
  (core:unsupported-feature "program name"))

(defun command-line-arguments ()
  #+(or sbcl ccl)
  (cdr (argv))
  #-(or sbcl ccl)
  (core:unsupported-feature "command line arguments"))
