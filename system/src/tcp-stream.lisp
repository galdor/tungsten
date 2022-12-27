(in-package :system)

(defclass tcp-stream (socket
                      streams:fundamental-binary-input-stream
                      streams:fundamental-binary-output-stream)
  ((read-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096))))

(defmethod streams:stream-clear-output ((stream tcp-stream))
  (with-slots (write-buffer) stream
    (core:buffer-reset write-buffer))
  nil)

(defmethod streams:stream-write-byte ((stream tcp-stream) octet)
  (declare (type core:octet octet))
  (with-slots (write-buffer) stream
    (core:buffer-append-octet write-buffer octet))
  octet)

(defmethod streams:stream-write-sequence ((stream tcp-stream) octets
                                          &optional (start 0) end)
  ;; Note that WRITE-SEQUENCE can call this method with END being NIL
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (write-buffer) stream
    (core:buffer-append-octets write-buffer octets
                               :start start :end (or end (length octets))))
  octets)

(defmethod streams:stream-force-output ((stream tcp-stream))
  (with-slots (file-descriptor write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (let ((nb-written
             (write-fd file-descriptor %data
                       (core:buffer-length write-buffer))))
        (core:buffer-skip write-buffer nb-written))))
  nil)

(defmethod streams:stream-finish-output ((stream tcp-stream))
  (with-slots (file-descriptor write-buffer)
      stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (do ()
          ((core:buffer-empty-p write-buffer)
           nil)
        (let ((nb-written (write-fd file-descriptor %data
                                    (core:buffer-length write-buffer))))
          (setf %data (ffi:pointer+ %data nb-written))
          (core:buffer-skip write-buffer nb-written)))))
  nil)

(defmethod streams:stream-read-byte ((stream tcp-stream))
  (with-slots (file-descriptor read-buffer) stream
    (when (core:buffer-empty-p read-buffer)
      (let* ((read-size 4096)
             (position (core:buffer-reserve read-buffer read-size)))
        (ffi:with-pinned-vector-data
            (%data (core:buffer-data read-buffer) position)
          (let ((nb-read (read-fd file-descriptor %data read-size)))
            (incf (core:buffer-end read-buffer) nb-read)
            (when (zerop nb-read)
              (return-from streams:stream-read-byte :eof))))))
    (prog1
        (aref (core:buffer-data read-buffer) (core:buffer-start read-buffer))
      (core:buffer-skip read-buffer 1))))

(defmethod streams:stream-read-sequence ((stream tcp-stream) octets
                                         &optional (start 0) end
                                         &aux (end (or end (length octets))))
  ;; Note that READ-SEQUENCE can call this method with END being NIL
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (file-descriptor read-buffer) stream
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
          (let ((nb-read (read-fd file-descriptor %data read-size)))
            (incf (core:buffer-end read-buffer) nb-read)
            (when (zerop nb-read)
              (setf eofp t))))))))
