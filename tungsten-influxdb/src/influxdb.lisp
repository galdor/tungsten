(in-package :influxdb)

(defclass point ()
  ((measurement
    :type string
    :initarg :measurement
    :accessor point-measurement)
   (tags
    :type list
    :initarg :tags
    :initform nil
    :accessor point-tags)
   (fields
    :type list
    :initarg :fields
    :initform nil
    :accessor point-fields)
   (timestamp
    :type (or time:datetime null)
    :initarg :timestamp
    :initform nil
    :accessor point-timestamp)))

(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (with-slots (measurement) point
      (prin1 measurement stream))))

(defun make-point (measurement
                   &key tags fields (timestamp (time:current-datetime)))
  (declare (type string measurement)
           (type list tags fields))
  (make-instance 'point :measurement measurement
                        :tags tags
                        :fields fields
                        :timestamp timestamp))
