(in-package :fonts)

(defmethod parse-table ((table name-table))
  (with-slots (version (table-offset offset)) table
    (setf version (parse-uint16 "version"))
    (let* ((count (parse-uint16 "count"))
           (relative-storage-offset (parse-offset16 "storageOffset"))
           (storage-offset (+ table-offset relative-storage-offset))
           (name-records nil))
      (setf (name-table-name-records table)
            (dotimes (i count (nreverse name-records))
              (push (parse-name-record storage-offset) name-records))))))

(defun parse-name-record (storage-offset)
  (declare (type (integer 0) storage-offset))
  (let* ((platform-id (parse-platform-id (parse-uint16 "platformID")))
         (encoding-id
           (parse-encoding-id (parse-uint16 "encodingID") platform-id))
         (language-id (parse-uint16 "languageID"))
         (name-id (parse-name-id (parse-uint16 "nameID")))
         (length (parse-uint16 "length"))
         (offset (parse-offset16 "stringOffset"))
         (octets-start (+ storage-offset offset))
         (octets-end (+ octets-start length))
         (value (decode-string (parser-data *parser*) platform-id encoding-id
                               :start octets-start :end octets-end)))
    (make-instance 'name-record :platform-id platform-id
                                :encoding-id encoding-id
                                :language-id language-id
                                :name-id name-id
                                :value value)))
