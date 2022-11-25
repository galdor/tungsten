(in-package :ffi)

(defvar *foreign-libraries* (make-hash-table :test #'equal)
  "The table containing all loaded shared libraries. Keys are objects comparable
with EQUAL which identify libraries. Values are cons cells containing the path
of the library and the handle returned by the low-level library loading
function.")

(defun use-foreign-library (name path)
  "Load the shared library at PATH if it is not already loaded and associate it
with symbol NAME. If a shared library named NAME is already loaded, unload the
old version and load the new one."
  (let ((info (gethash name *foreign-libraries*)))
    (cond
      ((null info)
       (setf (gethash name *foreign-libraries*)
             (cons path (%load-foreign-library path))))
      (t
       (%unload-foreign-library (cdr info))
       (setf (gethash name *foreign-libraries*)
             (cons path (%load-foreign-library path)))))))

(defun use-asdf-shared-library (name system-name component-name)
  "Load the shared library built by a SYSTEMS:SHARED-LIBRARY ASDF component. The
function behaves the same way as USE-FOREIGN-LIBRARY."
  (let ((system (asdf:find-system system-name)))
    (unless system
      (error "unknown ASDF system ~S" system-name))
    (let ((component (asdf:find-component system component-name)))
      (unless component
        (error "unknown ASDF component ~S in system ~S"
               component-name system-name))
      (let ((path (asdf:output-file 'asdf:compile-op component)))
        (ffi:use-foreign-library name path)))))
