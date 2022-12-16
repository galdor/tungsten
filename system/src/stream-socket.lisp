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
    :type core:octet-vector
    :initform (make-array 1024 :element-type 'core:octet))
   (write-buffer-start
    :type (integer 0)
    :initform 0)
   (write-buffer-end
    :type (integer 0)
    :initform 0)))

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
  (with-slots (write-buffer-length) socket
    (setf write-buffer-length 0))
  nil)

(defmethod streams:stream-write-byte ((socket stream-socket) octet)
  (declare (type stream-socket socket)
           (type core:octet octet))
  (with-slots (write-buffer write-buffer-end) socket
    (stream-socket-reserve-write-buffer-space socket 1)
    (setf (aref write-buffer write-buffer-end) octet)
    (incf write-buffer-end 1)))

(defmethod streams:stream-write-sequence ((socket stream-socket) octets
                                          &optional (start 0)
                                                    (end (length octets)))
  (declare (type stream-socket socket)
           (type core:octet-vector octets))
  (with-slots (write-buffer write-buffer-end) socket
    (let ((nb-octets (- end start)))
      (stream-socket-reserve-write-buffer-space socket nb-octets)
      (replace write-buffer octets :start1 write-buffer-end
                                   :start2 start :end2 end)
      (incf write-buffer-end nb-octets))))

(defun stream-socket-reserve-write-buffer-space (socket size)
  (declare (type stream-socket socket)
           (type (integer 0) size))
  (with-slots (write-buffer write-buffer-end) socket
    (unless (stream-socket-has-free-write-space socket size)
      (stream-socket-compact-write-buffer socket))
    (unless (stream-socket-has-free-write-space socket size)
      (stream-socket-resize-write-buffer
       socket (max size (* (length write-buffer) 2))))))

(defun stream-socket-resize-write-buffer (socket size)
  (declare (type stream-socket socket)
           (type (integer 1) size))
  (with-slots (write-buffer write-buffer-start write-buffer-end) socket
    (let ((write-buffer-2 (make-array size :element-type 'core:octet)))
      (replace write-buffer-2 write-buffer
               :start2 write-buffer-start :end2 write-buffer-end)
      (setf write-buffer write-buffer-2
            write-buffer-end (- write-buffer-end write-buffer-start)
            write-buffer-start 0))))

(defun stream-socket-compact-write-buffer (socket)
  (declare (type stream-socket socket))
  (with-slots (write-buffer write-buffer-start write-buffer-end) socket
    (when (> write-buffer-start 0)
      (replace write-buffer write-buffer
               :start2 write-buffer-start :end2 write-buffer-end))))

(defun stream-socket-has-free-write-space (socket n)
  (declare (type stream-socket socket)
           (type (integer 0) n))
  (with-slots (write-buffer write-buffer-end) socket
    (>= (- (length write-buffer) write-buffer-end) n)))

(defmethod streams:stream-force-output ((socket stream-socket))
  (with-slots (file-descriptor write-buffer
               write-buffer-start write-buffer-end)
      socket
    (ffi:with-pinned-vector-data (%data write-buffer)
      (let* ((%data (ffi:pointer+ %data write-buffer-start))
             (nb-written
               (write-fd file-descriptor %data
                         (- write-buffer-end write-buffer-start))))
        (incf write-buffer-start nb-written)))
    (when (= write-buffer-start write-buffer-end)
      (setf write-buffer-start 0
            write-buffer-end 0)))
  nil)

(defmethod streams:stream-finish-output ((socket stream-socket))
  (with-slots (file-descriptor write-buffer
               write-buffer-start write-buffer-end)
      socket
    (ffi:with-pinned-vector-data (%data write-buffer)
      (do ((%data (ffi:pointer+ %data write-buffer-start)))
          ((= write-buffer-start write-buffer-end)
           nil)
        (let ((nb-written (write-fd file-descriptor %data
                                    (- write-buffer-end write-buffer-start))))
          (setf %data (ffi:pointer+ %data nb-written))
          (incf write-buffer-start nb-written)))
      (setf write-buffer-start 0
            write-buffer-end 0)))
  nil)
