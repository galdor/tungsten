(in-package :system)

(defclass stream-socket (streams:fundamental-binary-input-stream
                         streams:fundamental-binary-output-stream)
  ((file-descriptor
    :type (or (integer 0) null)
    :initarg :file-descriptor
    :initform nil
    :reader stream-socket-file-descriptor)
   (address
    :type socket-address
    :initarg :address
    :reader stream-socket-address)
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 1024))))

(defmethod print-object ((socket stream-socket) stream)
  (print-unreadable-object (socket stream :type t)
    (with-slots (address) socket
      (write-string (format-socket-address address) stream))))

(defmethod close ((socket stream-socket) &key abort)
  (declare (ignore abort))
  (with-slots (file-descriptor) socket
    (when file-descriptor
      (close-fd file-descriptor)
      (setf file-descriptor nil)
      t)))

(defmethod open-stream-p ((socket stream-socket))
  (not (null (stream-socket-file-descriptor socket))))

(defmethod streams:stream-clear-output ((socket stream-socket))
  (with-slots (write-buffer) socket
    (core:buffer-reset write-buffer))
  nil)

(defmethod streams:stream-write-byte ((socket stream-socket) octet)
  (declare (type stream-socket socket)
           (type core:octet octet))
  (with-slots (write-buffer) socket
    (core:buffer-append-octet write-buffer 1))
  octet)

(defmethod streams:stream-write-sequence ((socket stream-socket) octets
                                          &optional start end)
  (declare (type stream-socket socket)
           (type core:octet-vector octets))
  (with-slots (write-buffer) socket
    (core:buffer-append-octets write-buffer octets
                               :start (or start 0)
                               :end (or end (length octets))))
  octets)

(defmethod streams:stream-force-output ((socket stream-socket))
  (with-slots (file-descriptor write-buffer)
      socket
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer))
      (let* ((%data (ffi:pointer+ %data (core:buffer-start write-buffer)))
             (nb-written
               (write-fd file-descriptor %data
                         (core:buffer-length write-buffer))))
        (core:buffer-skip write-buffer nb-written))))
  nil)

(defmethod streams:stream-finish-output ((socket stream-socket))
  (with-slots (file-descriptor write-buffer)
      socket
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer))
      (do ((%data (ffi:pointer+ %data (core:buffer-start write-buffer))))
          ((core:buffer-empty-p write-buffer)
           nil)
        (let ((nb-written (write-fd file-descriptor %data
                                    (core:buffer-length write-buffer))))
          (setf %data (ffi:pointer+ %data nb-written))
          (core:buffer-skip write-buffer nb-written)))))
  nil)
