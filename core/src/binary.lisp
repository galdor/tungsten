(in-package :core)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(array octet (,length)))

(defun octet-vector (data)
  (declare (type (or octet octet-vector vector list) data))
  (octet-vector* data))

(defun octet-vector* (&rest data)
  (let ((length (let ((n 0))
                  (dolist (datum data n)
                    (incf n (if (typep datum 'sequence) (length datum) 1))))))
    (let ((octets (make-array length :element-type 'octet))
          (start 0))
      (dolist (datum data octets)
        (etypecase datum
          (octet
           (setf (aref octets start) datum)
           (incf start 1))
          ((or octet-vector vector list)
           (replace octets datum :start1 start)
           (incf start (length datum))))))))
