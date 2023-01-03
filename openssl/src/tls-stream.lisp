(in-package :openssl)

(defclass tls-stream (system:network-stream)
  ((%ssl
    :type (or ffi:pointer null)
    :initarg :%ssl)))

(defmethod close :before ((stream tls-stream) &key abort)
  (declare (ignore abort))
  (with-slots (%ssl) stream
    (ignore-errors
     (ssl-shutdown %ssl))))

(defmethod close :after ((stream tls-stream) &key abort)
  (declare (ignore abort))
  (with-slots (%ssl) stream
    (when %ssl
      (ssl-free %ssl)
      (setf %ssl nil))))

(defmethod system:read-io-stream ((stream tls-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (%ssl) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (ssl-read %ssl %data (- end start))
        (openssl-error-stack (condition)
          (let* ((error (car (openssl-error-stack-errors condition)))
                 (reason (when error
                           (openssl-error-reason error))))
            (if (or (null error)
                    (eq reason :ssl-r-unexpected-eof-while-reading))
                0
                (error condition))))))))

(defmethod system:write-io-stream ((stream tls-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (%ssl) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (ssl-write %ssl %data (- end start)))))
