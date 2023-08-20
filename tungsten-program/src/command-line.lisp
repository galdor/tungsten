(in-package :program)

(defun argv ()
  #+sbcl
  sb-ext:*posix-argv*
  #+ccl
  ccl:*command-line-argument-list*
  #-(or sbcl ccl)
  (core:unsupported-feature "command line handling"))

(defun command-line-program-name ()
  #+(or sbcl ccl)
  (car (argv))
  #-(or sbcl ccl)
  (core:unsupported-feature "command line handling"))

(defun command-line-arguments ()
  #+(or sbcl ccl)
  (cdr (argv))
  #-(or sbcl ccl)
  (core:unsupported-feature "command line handling"))
