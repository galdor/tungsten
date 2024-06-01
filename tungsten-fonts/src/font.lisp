(in-package :fonts)

(defclass font ()
  ((data
    :type core:octet-vector
    :initarg :data
    :accessor font-data)
   (table-directory
    :type table-directory
    :accessor font-table-directory)
   (tables
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :reader font-tables)))

(defun load-font (path)
  (declare (type (or pathname string) path))
  (let ((data (system:read-file path)))
    (load-font-data data)))

(defun load-font-data (data)
  (declare (type core:octet-vector data))
  (let ((font (make-instance 'font :data data)))
    (setf (font-table-directory font) (decode-table-directory data))
    (decode-font-tables font)
    font))

(defun decode-font-tables (font)
  (declare (type font font))
  (with-slots (data table-directory) font
    (maphash
     (lambda (tag record)
       (restart-case
           (setf (gethash tag (font-tables font)) (decode-table record data))
         (ignore-table ()
           :report "ignore the table"
           nil)))
     (table-directory-tables table-directory))))

(defun font-table (font tag)
  (declare (type font font)
           (type tag tag))
  (or (gethash tag (font-tables font))
      (error 'unknown-table :tag tag)))
