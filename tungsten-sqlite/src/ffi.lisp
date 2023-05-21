(in-package :sqlite)

;;;
;;; Library
;;;

(ffi:use-asdf-shared-library 'sqlite "tungsten-sqlite" "sqlite")

(defun library-version ()
  (let ((%string (ffi:foreign-funcall "sqlite3_libversion" (() :pointer))))
    (ffi:decode-foreign-string %string)))
