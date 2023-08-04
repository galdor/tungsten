(in-package :libssh)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'ssh "libssh.so.4")

(defun library-version ()
  "Return a string containing the version number of the libssh library."
  ;; The version string is formatted as:
  ;;
  ;; version-string = version { "/" feature }
  (let* ((string (ffi:decode-foreign-string
                  (ffi:foreign-funcall "ssh_version" ((:int) :pointer) 0)))
         (version-end (or (position #\/ string) (length string))))
    (subseq string 0 version-end)))
