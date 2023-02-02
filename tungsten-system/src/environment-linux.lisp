(in-package :system)

(defun %count-file-descriptors ()
  (let* ((directory #p"/proc/self/fd/")
         (wild-path (make-pathname :name :wild :defaults directory))
         (entries (directory wild-path)))
    (length entries)))
