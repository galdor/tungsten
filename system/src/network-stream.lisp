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
    :initform (core:make-buffer 4096))
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))))

(defgeneric read-network-stream (stream octets start end)
  (:documentation
   "Attempt to read the stream to an octet vector and return the number of octets
actually read. Return 0 if end-of-file was reached."))

(defgeneric write-network-stream (stream octets start end)
  (:documentation
   "Attempt to write an octet vector to the stream and return the number of
octets actually written."))

(defmethod close ((stream network-stream) &key abort)
  (declare (ignore abort))
  (with-slots (socket) stream
    (when socket
      (close-fd socket)
      (setf socket nil)
      t)))

(defmethod open-stream-p ((stream network-stream))
  (not (null (network-stream-socket stream))))

(defmethod streams:stream-read-byte ((stream network-stream))
  (with-slots (socket read-buffer) stream
    (when (core:buffer-empty-p read-buffer)
      (let* ((read-size 4096)
             (position (core:buffer-reserve read-buffer read-size)))
        (let ((nb-read (read-network-stream stream
                                            (core:buffer-data read-buffer)
                                            position
                                            (+ position read-size))))
          (incf (core:buffer-end read-buffer) nb-read)
          (when (zerop nb-read)
            (return-from streams:stream-read-byte :eof)))))
    (prog1
        (aref (core:buffer-data read-buffer) (core:buffer-start read-buffer))
      (core:buffer-skip read-buffer 1))))

(defmethod streams:stream-read-sequence ((stream network-stream) octets
                                         &optional (start 0) end
                                         &aux (end (or end (length octets))))
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (socket read-buffer) stream
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
             (position (core:buffer-reserve read-buffer read-size))
             (nb-read (read-network-stream stream
                                           (core:buffer-data read-buffer)
                                           position
                                           (+ position read-size))))
        (incf (core:buffer-end read-buffer) nb-read)
        (when (zerop nb-read)
          (setf eofp t))))))

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

(defmethod streams:stream-force-output ((stream network-stream))
  (with-slots (socket write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (let ((nb-written
              (write-network-stream stream
                                    (core:buffer-data write-buffer)
                                    (core:buffer-start write-buffer)
                                    (core:buffer-end write-buffer))))
        (core:buffer-skip write-buffer nb-written))))
  nil)

(defmethod streams:stream-finish-output ((stream network-stream))
  (with-slots (socket write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (do ()
          ((core:buffer-empty-p write-buffer)
           nil)
        (let ((nb-written
                (write-network-stream stream
                                      (core:buffer-data write-buffer)
                                      (core:buffer-start write-buffer)
                                      (core:buffer-end write-buffer))))
          (setf %data (ffi:pointer+ %data nb-written))
          (core:buffer-skip write-buffer nb-written)))))
  nil)
