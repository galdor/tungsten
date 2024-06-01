(in-package :fonts)

(defclass table-directory ()
  ((sfnt-version
    :type tag
    :initarg :sfnt-version
    :reader table-directory-sfnt-version)
   (tables
    :type hash-table
    :initarg :tables
    :initform (make-hash-table :test #'equal)
    :reader table-directory-tables)))

(defun decode-table-directory (data &key (start 0))
  (declare (type core:octet-vector data)
           (type (integer 0) start))
  (with-decoder (data :start start :end (min (+ start 12) (length data))
                      :context "table directory")
    (let* ((version (read-field/tag "sfntVersion"))
           (directory (make-instance 'table-directory :sfnt-version version))
           (nb-tables (read-field/uint16 "numTables")))
      (read-field/uint16 "searchRange")
      (read-field/uint16 "entrySelector")
      (read-field/uint16 "rangeShift")
      (incf start 12)
      (dotimes (i nb-tables)
        (let ((record (decode-table-record data :start start)))
          (setf (gethash (table-record-tag record)
                         (table-directory-tables directory))
                record)
          (incf start 16)))
      directory)))

(defclass table-record ()
  ((tag
    :type tag
    :initarg :tag
    :accessor table-record-tag)
   (checksum
    :type (unsigned-byte 32)
    :initarg :checksum
    :accessor table-record-checksum)
   (offset
    :type (unsigned-byte 32)
    :initarg :offset
    :accessor table-record-offset)
   (length
    :type (unsigned-byte 32)
    :initarg :length
    :accessor table-record-length)))

(defmethod print-object ((record table-record) stream)
  (print-unreadable-object (record stream :type t)
    (write-string (table-record-tag record) stream)))

(defun decode-table-record (data &key (start 0))
  (declare (type core:octet-vector data)
           (type (integer 0) start))
  (with-decoder (data :start start :end (min (+ start 16) (length data))
                      :context "table record")
    (let ((record (make-instance 'table-record)))
      (setf (table-record-tag record) (read-field/tag "tableTag"))
      (setf (table-record-checksum record) (read-field/uint32 "checksum"))
      (setf (table-record-offset record) (read-field/offset32 "offset"))
      (setf (table-record-length record) (read-field/uint32 "length"))
      record)))
