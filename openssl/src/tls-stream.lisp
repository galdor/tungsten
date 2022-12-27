(in-package :openssl)

(defclass tls-stream (system:socket
                      streams:fundamental-binary-input-stream
                      streams:fundamental-binary-output-stream)
  ((%ssl
    :type (or ffi:pointer null)
    :initarg :%ssl)
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))))

(defmethod close :after ((stream tls-stream) &key abort)
  (declare (ignore abort))
  (with-slots (%ssl) stream
    (when %ssl
      (ssl-free %ssl)
      (setf %ssl nil))))

(defmethod streams:stream-clear-output ((stream tls-stream))
  (with-slots (write-buffer) stream
    (core:buffer-reset write-buffer))
  nil)

(defmethod streams:stream-write-byte ((stream tls-stream) octet)
  (declare (type core:octet octet))
  (with-slots (write-buffer) stream
    (core:buffer-append-octet write-buffer octet))
  octet)

(defmethod streams:stream-write-sequence ((stream tls-stream) octets
                                          &optional (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (write-buffer) stream
    (core:buffer-append-octets write-buffer octets
                               :start start :end (or end (length octets))))
  octets)

(defmethod streams:stream-force-output ((stream tls-stream))
  (with-slots (%ssl write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (let ((nb-written
              (ssl-write %ssl %data (core:buffer-length write-buffer))))
        (core:buffer-skip write-buffer nb-written))))
  nil)

(defmethod streams:stream-finish-output ((stream tls-stream))
  (with-slots (%ssl write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (do ()
          ((core:buffer-empty-p write-buffer)
           nil)
        (let ((nb-written
                (ssl-write %ssl %data (core:buffer-length write-buffer))))
          (setf %data (ffi:pointer+ %data nb-written))
          (core:buffer-skip write-buffer nb-written)))))
  nil)
