(in-package :ucd)

(defparameter *ucd-path*
  (asdf:system-relative-pathname "tungsten-text" #p"data/ucd/"))

(defun ucd-file-path (subpath)
  (declare (type (or pathname string) subpath))
  (merge-pathnames subpath *ucd-path*))
