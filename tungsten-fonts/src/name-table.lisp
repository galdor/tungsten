(in-package :fonts)

(define-table (name-table "name") ()
  ((version
    :type (unsigned-byte 16)
    :accessor name-table-version)
   (records
    :type list
    :accessor name-table-records)))

(defclass name-record ()
  ((platform-id
    :type (or symbol (unsigned-byte 16))
    :initarg :platform-id
    :accessor name-record-platform-id)
   (encoding-id
    :type (or symbol (unsigned-byte 16))
    :initarg :encoding-id
    :accessor name-record-encoding-id)
   (language-id
    :type (or symbol (unsigned-byte 16))
    :initarg :language-id
    :accessor name-record-language-id)
   (name-id
    :type (or symbol (unsigned-byte 16))
    :initarg :name-id
    :accessor name-record-name-id)
   (value
    :type (or string core:octet-vector)
    :initarg :value
    :accessor name-record-value)))

(defmethod print-object ((record name-record) stream)
  (print-unreadable-object (record stream :type t)
    (with-slots (name-id value) record
      (format stream "~A ~S" name-id value))))

(defmethod decode-table-data ((table name-table) record)
  (declare (type table-record record))
  (setf (name-table-version table) (read-field/uint16 "version"))
  (let* ((nb-records (read-field/uint16 "count"))
         (storage-start (+ (table-record-offset record)
                           (read-field/uint16 "storageOffset")))
         (records nil))
    (setf (name-table-records table)
          (dotimes (i nb-records (nreverse records))
            (push (decode-name-record storage-start) records)))))

(defun decode-name-record (storage-start)
  (declare (type (integer 0) storage-start))
  (let* ((platform-id (decode-platform-id (read-field/uint16 "platformID")))
         (encoding-id
           (decode-encoding-id (read-field/uint16 "encodingID") platform-id))
         (language-id (read-field/uint16 "languageID"))
         (name-id (decode-name-id (read-field/uint16 "nameID")))
         (value-length (read-field/uint16 "length"))
         (value-start (+ storage-start (read-field/offset16 "stringOffset")))
         (value-end (+ value-start value-length))
         (value (decode-string (decoder-data *decoder*) platform-id encoding-id
                               :start value-start :end value-end)))
    (make-instance 'name-record :platform-id platform-id
                                :encoding-id encoding-id
                                :language-id language-id
                                :name-id name-id
                                :value value)))
