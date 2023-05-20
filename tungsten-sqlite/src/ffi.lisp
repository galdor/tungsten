(in-package :sqlite)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'sqlite "libsqlite3.so")

(defun library-version ()
  (let ((%string (ffi:foreign-funcall "sqlite3_libversion" (() :pointer))))
    (ffi:decode-foreign-string %string)))
