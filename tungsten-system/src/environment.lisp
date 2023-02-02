(in-package :system)

(defun hostname ()
  "Return the hostname of the system."
  (gethostname))

(defun count-file-descriptors ()
  "Return the number of file descriptor open by the current process."
  (%count-file-descriptors))

(defun file-descriptor-limit ()
  "Return the maximum number of file descriptors for the current process or NIL
if there is no limit."
  (multiple-value-bind (current max)
      (getrlimit :rlimit-nofile)
    (declare (ignore max))
    (when (integerp current)
      current)))

(defun memory-page-size ()
  "Return the size of a memory page in bytes."
  (sysconf :sc-pagesize))

(defun memory-usage ()
  "Return three values indicating the amount of memory used by the current
process: the amount of virtual memory, the amount of resident memory and the
amount of shared memory. All values are specified in bytes."
  (%memory-usage))

