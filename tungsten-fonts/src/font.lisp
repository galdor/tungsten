(in-package :fonts)

(defclass font ()
  ((table-directory
    :type table-directory
    :initarg :table-directory
    :accessor font-table-directory)))

(defun load-font (path)
  (declare (type (or pathname string) path))
  (parse-font (system:read-file path)))

(defun parse-font (data)
  (declare (type core:octet-vector data))
  (let* ((*parser*
           (make-instance 'parser :data data :start 0 :end (length data)))
         (table-directory (parse-table-directory)))
    (make-instance 'font :table-directory table-directory)))

(defun font-table (font name)
  (declare (type font font)
           (type tag name))
  (table-directory-table (font-table-directory font) name))

(defun font-ascender (font)
  (declare (type font font))
  (hhea-table-ascender (font-table font "hhea")))

(defun font-descender (font)
  (declare (type font font))
  (hhea-table-descender (font-table font "hhea")))

(defun font-line-gap (font)
  (declare (type font font))
  (hhea-table-line-gap (font-table font "hhea")))
