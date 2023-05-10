(in-package :core)

(define-condition unavailable-random-data ()
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Random data currently unavailable."))))

(defun random-octets (nb-octets)
  (let ((data (make-octet-vector nb-octets)))
    (with-open-file (stream "/dev/urandom" :element-type 'core:octet)
      (let ((nb-read (read-sequence data stream)))
        (when (< nb-read nb-octets)
          (error 'unavailable-random-data))))
    data))
