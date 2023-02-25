(in-package :core)

(defun macroexpand-all (form &optional environment)
  #+sbcl
  (sb-cltl2:macroexpand-all form environment)
  #+ccl
  (ccl:macroexpand-all form env)
  #-(or sbcl)
  (unsupported-feature "full macro-expansion"))
