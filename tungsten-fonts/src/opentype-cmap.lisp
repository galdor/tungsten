(in-package :fonts)

(defclass cmap-table (table)
  ((version
     :type uint16
     :initarg :version
     :accessor cmap-table-version)
   (encoding-records
    :type list
    :accessor cmap-table-encoding-records)))

(defclass encoding-record ()
  ((platform-id
    :type (or symbol uint16)
    :initarg :platform-id
    :accessor encoding-record-platform-id)
   (encoding-id
    :type (or symbol uint16)
    :initarg :encoding-id
    :accessor encoding-record-encoding-id)
   (subtable-offset
    :type offset32
    :initarg :subtable-offset
    :accessor encoding-record-subtable-offset)))

(defmethod parse-table ((table cmap-table))
  (setf (cmap-table-version table) (parse-uint16 "version"))
  (let* ((nb-encoding-records (parse-uint16 "numTables"))
         (encoding-records nil))
    (setf (cmap-table-encoding-records table)
          (dotimes (i nb-encoding-records (nreverse encoding-records))
            (push (parse-encoding-record) encoding-records)))))

(defun parse-encoding-record ()
  (let* ((platform-id (parse-platform-id (parse-uint16 "platformID")))
         (encoding-id
           (parse-encoding-id (parse-uint16 "encodingID") platform-id))
         (subtable-offset (parse-offset32 "subtableOffset")))
    (make-instance 'encoding-record :platform-id platform-id
                                    :encoding-id encoding-id
                                    :subtable-offset subtable-offset)))
