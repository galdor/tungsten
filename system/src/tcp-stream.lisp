(in-package :system)

(defclass tcp-stream (network-stream)
  ())

(defmethod read-network-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (socket) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (read-fd socket %data (- end start)))))

(defmethod write-network-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (socket write-buffer) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (write-fd socket %data (- end start)))))
