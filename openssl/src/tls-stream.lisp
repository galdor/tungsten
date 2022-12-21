(in-package :openssl)

(defclass tls-stream (system:socket
                      streams:fundamental-binary-input-stream
                      streams:fundamental-binary-output-stream)
  ((%ssl
    :type (or ffi:pointer null)
    :initarg :%ssl)))

(defmethod close :after ((stream tls-stream) &key abort)
  (declare (ignore abort))
  (with-slots (%ssl) stream
    (when %ssl
      (ssl-free %ssl)
      (setf %ssl nil))))
