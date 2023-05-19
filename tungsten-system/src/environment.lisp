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
  "Return two values indicating the amount of memory used by the current
process: the amount of virtual memory and the amount of resident memory. All
values are specified in bytes."
  (%memory-usage))

(defun environment-variable (name)
  "Return the value of the environment variable NAME or NIL if there is no
environment variable with this name."
  (handler-case
      (getenv name)
    (system-error (condition)
      (declare (ignore condition))
      nil)))

(defun home-directory-path ()
  "Return the path of the home directory of the current user"
  (let ((namestring (environment-variable "HOME")))
    (when namestring
      (directory-path (pathname namestring)))))
