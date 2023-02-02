(in-package :system)

(defun hostname ()
  "Return the hostname of the system."
  (gethostname))

(defun file-descriptor-limit ()
  "Return the maximum number of file descriptors for the current process or NIL
if there is no limit."
  (multiple-value-bind (current max)
      (getrlimit :rlimit-nofile)
    (declare (ignore max))
    (when (integerp current)
      current)))
