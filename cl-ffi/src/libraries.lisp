(in-package :ffi)

(defvar *foreign-libraries* (make-hash-table :test #'equal)
  "The table containing all loaded shared libraries. Keys are objects comparable
with EQUAL which identify libraries. Values are cons cells containing the path
of the library and the handle returned by the low-level library loading
function.")

(defun use-foreign-library (name path)
  "Load the shared library at PATH if it is not already loaded and associate it
with symbol NAME. If a shared library named NAME is already loaded with a
different path, unload the old version and load the new one."
  (let ((info (gethash name *foreign-libraries*)))
    (cond
      ((null info)
       (setf (gethash name *foreign-libraries*)
             (cons path (%load-foreign-library path))))
      ((equal path (car info))
       info)
      (t
       (%unload-foreign-library (cdr info))
       (setf (gethash name *foreign-libraries*)
             (cons path (%load-foreign-library path)))))))
