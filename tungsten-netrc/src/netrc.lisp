(in-package :netrc)

(defparameter *auto-load-entries* t)

(defvar *entries* nil)

(defun default-path ()
  (let ((namestring (system:environment-variable "NETRC")))
    (if namestring
        (pathname namestring)
        (let ((home-path (system:home-directory-path)))
          (if home-path
              (merge-pathnames #p".netrc" home-path)
              nil)))))

(defun load-entries* (&optional (path (default-path))
                      &key (if-does-not-exist :error))
  (declare (type (or pathname string) path)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (let ((data (system:read-file path :external-format text:*default-encoding*
                                     :if-does-not-exist if-does-not-exist)))
    (when data
      (parse-entries data))))

(defun load-entries (&optional (path (default-path)))
  (declare (type (or pathname string) path))
  (setf *entries* (load-entries* path :if-does-not-exist nil)))

(defun search-entries (&rest args &key machine port login account)
  (declare (type (or string null) machine login account)
           (type (or (integer 1 65535) null) port)
           (ignore machine port login account))
  (when (and (null *entries*) *auto-load-entries*)
    (load-entries))
  (apply 'search-entries* *entries* args))
