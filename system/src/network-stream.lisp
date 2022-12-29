(in-package :system)

(defclass network-stream (streams:fundamental-binary-input-stream
                          streams:fundamental-binary-output-stream)
  ((socket
    :type (or (integer 0) null)
    :initarg :socket
    :initform nil
    :reader network-stream-socket)
   (address
    :type socket-address
    :initarg :address
    :reader network-stream-address)
   (read-buffer
    :type core:buffer
    :initform (core:make-buffer 4096)
    :accessor network-stream-read-buffer)
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096)
    :accessor network-stream-write-buffer)))

(defmethod close ((stream network-stream) &key abort)
  (declare (ignore abort))
  (with-slots (socket) stream
    (when socket
      (close-fd socket)
      (setf socket nil)
      t)))

(defmethod open-stream-p ((stream network-stream))
  (not (null (network-stream-socket stream))))

(defmethod streams:stream-clear-output ((stream network-stream))
  (with-slots (write-buffer) stream
    (core:buffer-reset write-buffer))
  nil)

(defmethod streams:stream-write-byte ((stream network-stream) octet)
  (declare (type core:octet octet))
  (with-slots (write-buffer) stream
    (core:buffer-append-octet write-buffer octet))
  octet)

(defmethod streams:stream-write-sequence ((stream network-stream) octets
                                          &optional (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (write-buffer) stream
    (core:buffer-append-octets write-buffer octets
                               :start start :end (or end (length octets))))
  octets)
