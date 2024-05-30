(in-package :fonts)

(defclass font ()
  ((data
    :type core:octet-vector
    :initarg :data
    :reader font-data)
   (table-directory
    :type table-directory
    :reader font-table-directory)))

(defun load-font (path)
  (declare (type (or pathname string) path))
  (let ((data (system:read-file path)))
    (load-font-data data)))

(defun load-font-data (data)
  (declare (type core:octet-vector data))
  (let ((font (make-instance 'font :data data)))
    (setf (slot-value font 'table-directory)
          (decode-table-directory data))
    font))
