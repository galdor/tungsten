(in-package :system)

(defclass tcp-stream (network-stream)
  ())

(defmethod close :before ((stream tcp-stream) &key abort)
  (declare (ignore abort))
  (with-slots (file-descriptor) stream
    (ignore-errors
     (shutdown file-descriptor :shut-rdwr))))

(defmethod read-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (file-descriptor) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (read-fd file-descriptor %data (- end start)))))

(defmethod write-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (file-descriptor write-buffer) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (write-fd file-descriptor %data (- end start))
        (system-error (condition)
          (case (system-error-value condition)
            (:epipe
             (error 'end-of-file :stream stream))
            (t
             (error condition))))))))
