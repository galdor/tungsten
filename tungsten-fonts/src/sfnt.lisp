(in-package :fonts)

;;; Reference: https://learn.microsoft.com/en-us/typography/opentype/spec/
;;;
;;; As TrueType, OpenType derives from SFNT so we can use it as reference.

(defvar *field-stream* nil)

(defmacro with-field-stream ((data &key (start 0) end) &body body)
  `(streams:with-input-from-octet-vector
       (*field-stream* ,data :start ,start :end ,end
                       :external-format :ascii)
     ,@body))

(defclass table-directory ()
  ((sfnt-version
    :type (string 4)
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
  (with-field-stream (data :start start :end (min (+ start 12) (length data)))
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
    :type (string 4)
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
  (with-field-stream (data :start start :end (min (+ start 16) (length data)))
    (let ((record (make-instance 'table-record)))
      (setf (table-record-tag record) (read-field/tag "tableTag"))
      (setf (table-record-checksum record) (read-field/uint32 "checksum"))
      (setf (table-record-offset record) (read-field/offset32 "offset"))
      (setf (table-record-length record) (read-field/uint32 "length"))
      record)))

(define-condition truncated-field (error)
  ((name
    :type string
    :initarg :name
    :reader truncated-field-name)
   (size
    :type (integer 0)
    :initarg :size
    :reader truncated-field-size))
  (:report
   (lambda (condition stream)
     (format stream "truncated field ~S (~D octets)"
             (truncated-field-name condition)
             (truncated-field-size condition)))))

(defmacro define-field-reader ((type-name type-size) (data) &body body)
  (let ((function-name
          (concatenate 'string "READ-FIELD/" (symbol-name type-name)))
        (nb-read (gensym "NB-READ-")))
    `(defun ,(intern function-name) (name)
       (declare (type string name))
       (let* ((,data (core:make-octet-vector ,type-size))
              (,nb-read (read-sequence ,data *field-stream*)))
         (when (< ,nb-read ,type-size)
           (error 'truncated-field :name name :size ,type-size))
         ,@body))))

(define-field-reader (#:uint16 2) (data)
  (core:binref :uint16be data))

(define-field-reader (#:uint32 4) (data)
  (core:binref :uint32be data))

(define-field-reader (#:offset32 4) (data)
  (core:binref :uint32be data))

(define-field-reader (#:tag 4) (data)
  (text:decode-string data))
