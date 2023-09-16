(in-package :core)

(defun halt (&optional (code 0))
  (declare (type (unsigned-byte 8) code))
  #+sbcl (sb-ext:exit :code code)
  #+ccl  (ccl:quit code))
