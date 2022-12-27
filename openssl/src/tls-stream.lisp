(in-package :openssl)

(defclass tls-stream (system:socket
                      streams:fundamental-binary-input-stream
                      streams:fundamental-binary-output-stream)
  ((%ssl
    :type (or ffi:pointer null)
    :initarg :%ssl)
   (read-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))))

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

(defmethod streams:stream-read-byte ((stream tls-stream))
  (with-slots (%ssl read-buffer) stream
    (when (core:buffer-empty-p read-buffer)
      (let* ((read-size 4096)
             (position (core:buffer-reserve read-buffer read-size)))
        (ffi:with-pinned-vector-data
            (%data (core:buffer-data read-buffer) position)
          (let ((nb-read (ssl-read-with-eof-detection %ssl %data read-size)))
            (when (eq nb-read :eof)
              (return-from streams:stream-read-byte :eof))
            (incf (core:buffer-end read-buffer) nb-read)))))
    (prog1
        (aref (core:buffer-data read-buffer) (core:buffer-start read-buffer))
      (core:buffer-skip read-buffer 1))))

(defmethod streams:stream-read-sequence ((stream tls-stream) octets
                                         &optional (start 0) end
                                         &aux (end (or end (length octets))))
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (%ssl read-buffer) stream
    (do ((nb-octets (- end start))
         (eofp nil))
        ((or eofp (>= (core:buffer-length read-buffer) nb-octets))
         (let* ((buffer-start (core:buffer-start read-buffer))
                (buffer-end (min (core:buffer-end read-buffer)
                                 (+ buffer-start nb-octets))))
           (replace octets (core:buffer-data read-buffer)
                    :start1 start :end1 end
                    :start2 buffer-start :end2 buffer-end)
           (core:buffer-skip read-buffer (- buffer-end buffer-start))
           buffer-end))
      (let* ((read-size
              (max (- (core:buffer-length read-buffer) nb-octets) 4096))
             (position (core:buffer-reserve read-buffer read-size)))
        (ffi:with-pinned-vector-data
            (%data (core:buffer-data read-buffer) position)
          (let ((nb-read (ssl-read-with-eof-detection %ssl %data read-size)))
            (cond
              ((eq nb-read :eof)
               (setf eofp t))
              (t
               (incf (core:buffer-end read-buffer) nb-read)))))))))

(defun ssl-read-with-eof-detection (%ssl %data size)
  (handler-case
      (ssl-read %ssl %data size)
    (openssl-error-stack (condition)
      (let* ((error (car (openssl-error-stack-errors condition)))
             (reason (when error
                       (openssl-error-reason error))))
        (if (or (null error)
                (eq reason :ssl-r-unexpected-eof-while-reading))
            :eof
            (error condition))))))
