(in-package :netrc)

(defun default-path ()
  (let ((namestring (system:environment-variable "NETRC")))
    (if namestring
        (pathname namestring)
        (let ((home-path (system:home-directory-path)))
          (if home-path
              (merge-pathnames #p".netrc" home-path)
              nil)))))

(defun load-entries (path)
  (declare (type (or pathname string) path))
  (parse-entries
   (system:read-file path :external-format text:*default-encoding*)))
