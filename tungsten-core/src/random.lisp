(in-package :core)

(define-condition unavailable-random-data (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "random data currently unavailable"))))

(defun random-octets (nb-octets)
  (let ((data (make-octet-vector nb-octets)))
    (with-open-file (stream "/dev/urandom" :element-type 'core:octet)
      (let ((nb-read (read-sequence data stream)))
        (when (< nb-read nb-octets)
          (error 'unavailable-random-data))))
    data))
