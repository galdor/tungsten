(in-package :fonts)

(defvar *table-classes* (make-hash-table :test #'equal))

(define-condition unknown-table (error)
  ((tag
    :type tag
    :initarg :tag
    :reader unknown-table-tag))
  (:report
   (lambda (condition stream)
     (format stream "unknown table ~S" (unknown-table-tag condition)))))

(defun table-class (tag)
  (declare (type tag tag))
  (or (gethash tag *table-classes*)
      (error 'unknown-table :tag tag)))

(defclass table ()
  ((tag
    :type tag
    :initarg :tag
    :accessor table-tag)))

;; We have no reason to make table classes inherit from any other class than
;; TABLE, but allowing it mimics a DEFCLASS form and leads to the form being
;; correctly indented.
(defmacro define-table ((class-name tag)
                        (&rest superclass-names)
                        slot-specifiers)
  `(progn
     (defclass ,class-name (table ,@superclass-names)
       ,slot-specifiers
       (:default-initargs
        :tag ,tag))
     (setf (gethash ,tag *table-classes*) ',class-name)))

(defgeneric decode-table-data (table record))

(defun decode-table (record data)
  (declare (type table-record record)
           (type core:octet-vector data))
  (with-slots (tag offset length) record
    (with-decoder (data :start offset :end (+ offset length)
                        :context (format nil "table ~S" tag))
      (let* ((class (handler-bind
                        ((unknown-table
                           (core:restart-condition-handler 'ignore-table)))
                      (table-class tag)))
             (table (make-instance class)))
        (restart-case
            (decode-table-data table record)
          (continue ()
            :report "continue with the partially decoded table"
            nil))
        table))))
