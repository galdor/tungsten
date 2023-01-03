(in-package :system)

(defclass io-stream (streams:fundamental-binary-input-stream
                     streams:fundamental-character-input-stream
                     streams:fundamental-binary-output-stream
                     streams:fundamental-character-output-stream)
  ((file-descriptor
    :type (or (integer 0) null)
    :initarg :file-descriptor
    :initform nil
    :reader io-stream-file-descriptor)
   (read-buffer
    :type core:buffer
    :initform (core:make-buffer 4096)
    :reader io-stream-read-buffer)
   (write-buffer
    :type core:buffer
    :initform (core:make-buffer 4096)
    :reader io-stream-write-buffer)
   (external-format
    :type text:external-format
    :initarg :external-format
    :initform text:*default-external-format*
    :reader io-stream-external-format)))

(defgeneric read-io-stream (stream octets start end)
  (:documentation
   "Read data from an IO stream to an octet vector and return the number of
octets read. Return 0 if end-of-file was reached."))

(defgeneric write-io-stream (stream octets start end)
  (:documentation
   "Write an octet vector to an IO stream and return the number of octets
written."))

(defmethod close ((stream io-stream) &key abort)
  (declare (ignore abort))
  (with-slots (file-descriptor) stream
    (when file-descriptor
      (close-fd file-descriptor)
      (setf file-descriptor nil)
      t)))

(defmethod open-stream-p ((stream io-stream))
  (not (null (io-stream-file-descriptor stream))))

(defmethod streams:stream-read-byte ((stream io-stream))
  (with-slots (file-descriptor read-buffer) stream
    (when (core:buffer-empty-p read-buffer)
      (let* ((read-size 4096)
             (position (core:buffer-reserve read-buffer read-size)))
        (let ((nb-read (read-io-stream stream
                                       (core:buffer-data read-buffer)
                                       position
                                       (+ position read-size))))
          (incf (core:buffer-end read-buffer) nb-read)
          (when (zerop nb-read)
            (return-from streams:stream-read-byte :eof)))))
    (prog1
        (aref (core:buffer-data read-buffer) (core:buffer-start read-buffer))
      (core:buffer-skip read-buffer 1))))

(defmethod streams:stream-read-sequence ((stream io-stream) octets
                                         &optional (start 0) end
                                         &aux (end (or end (length octets))))
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (with-slots (file-descriptor read-buffer) stream
    (do ((nb-octets (- end start))
         (eof nil))
        ((or eof (>= (core:buffer-length read-buffer) nb-octets))
         (let* ((buffer-start (core:buffer-start read-buffer))
                (buffer-end (min (core:buffer-end read-buffer)
                                 (+ buffer-start nb-octets))))
           (replace octets (core:buffer-data read-buffer)
                    :start1 start :end1 end
                    :start2 buffer-start :end2 buffer-end)
           (core:buffer-skip read-buffer (- buffer-end buffer-start))
           buffer-end))
      (when (zerop (io-stream-read-more stream))
        (setf eof t)))))

(defmethod streams:stream-clear-output ((stream io-stream))
  (with-slots (write-buffer) stream
    (core:buffer-reset write-buffer))
  nil)

(defmethod streams:stream-write-byte ((stream io-stream) octet)
  (declare (type core:octet octet))
  (with-slots (write-buffer) stream
    (core:buffer-append-octet write-buffer octet))
  octet)

(defmethod streams:stream-write-sequence ((stream io-stream) octets
                                          &optional (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (when (> (length octets) 0)
    (with-slots (write-buffer) stream
      (core:buffer-append-octets write-buffer octets
                                 :start start :end (or end (length octets)))))
  octets)

(defmethod streams:stream-force-output ((stream io-stream))
  (with-slots (file-descriptor write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (let ((nb-written
              (write-io-stream stream
                               (core:buffer-data write-buffer)
                               (core:buffer-start write-buffer)
                               (core:buffer-end write-buffer))))
        (core:buffer-skip write-buffer nb-written))))
  nil)

(defmethod streams:stream-finish-output ((stream io-stream))
  (with-slots (file-descriptor write-buffer) stream
    (ffi:with-pinned-vector-data (%data (core:buffer-data write-buffer)
                                        (core:buffer-start write-buffer))
      (do ()
          ((core:buffer-empty-p write-buffer)
           nil)
        (let ((nb-written
                (write-io-stream stream
                                 (core:buffer-data write-buffer)
                                 (core:buffer-start write-buffer)
                                 (core:buffer-end write-buffer))))
          (setf %data (ffi:pointer+ %data nb-written))
          (core:buffer-skip write-buffer nb-written)))))
  nil)

(defmethod streams:stream-read-char ((stream io-stream))
  (with-slots (read-buffer external-format) stream
    (do ((encoding (text:external-format-encoding external-format)))
        (nil)
      (multiple-value-bind (character nb-octets)
          (text:decode-character (core:buffer-data read-buffer)
                                 :encoding encoding
                                 :start (core:buffer-start read-buffer)
                                 :end (core:buffer-end read-buffer))
        (when character
          (core:buffer-skip read-buffer nb-octets)
          (return character))
        (let* ((read-size 4096)
               (position (core:buffer-reserve read-buffer read-size)))
          (let ((nb-read (read-io-stream stream
                                         (core:buffer-data read-buffer)
                                         position
                                         (+ position read-size))))
            (incf (core:buffer-end read-buffer) nb-read)
            (when (zerop nb-read)
              (return-from streams:stream-read-char :eof))))))))

(defmethod streams:stream-unread-char ((stream io-stream) character)
  (declare (type (character character)))
  (with-slots (read-buffer external-format) stream
    (let* ((encoding (text:external-format-encoding external-format))
           (nb-octets
             (text:encoded-character-length character :encoding encoding))
           (position
             (core:buffer-reserve-start read-buffer nb-octets)))
      (text:encode-string (string character)
                          :encoding encoding
                          :octets (core:buffer-data read-buffer)
                          :offset position)
      (decf (core:buffer-start read-buffer) nb-octets)))
  nil)

(defmethod streams:stream-read-char-no-hang ((stream io-stream))
  ;; This will not return a character if the read buffer is empty, even if it
  ;; could be possible to read the file descriptor and obtain at least one
  ;; character without blocking.
  ;;
  ;; The right way to implement this would be to switch the file descriptor to
  ;; non-blocking mode, perform one read operation, then switch back to
  ;; blocking mode.
  (with-slots (read-buffer external-format) stream
    (let ((encoding (text:external-format-encoding external-format)))
      (multiple-value-bind (character nb-octets)
          (text:decode-character (core:buffer-data read-buffer)
                                 :encoding encoding
                                 :start (core:buffer-start read-buffer)
                                 :end (core:buffer-end read-buffer))
        (when character
          (core:buffer-skip read-buffer nb-octets)
          character)))))

(defmethod streams:stream-peek-char ((stream io-stream))
  (let ((character (streams:stream-read-char stream)))
    (unless (eq character :eof)
      (streams:stream-unread-char stream character))
    character))

(defmethod streams:stream-listen ((stream io-stream))
  (let ((character (streams:stream-read-char-no-hang stream)))
    (cond
      ((or (null character)
           (eq character :eof))
       nil)
      (t
       (streams:stream-unread-char stream character)
       t))))

(defmethod streams:stream-read-line ((stream io-stream))
  (with-slots (read-buffer external-format) stream
    (do* ((encoding (text:external-format-encoding external-format))
          (eol-style (text:external-format-eol-style external-format))
          (eol-octets (text:eol-octets eol-style)))
         (nil)
      (let ((eol (search eol-octets (core:buffer-data read-buffer)
                         :start2 (core:buffer-start read-buffer)
                         :end2 (core:buffer-end read-buffer))))
        (when eol
          (let ((line
                  (text:decode-string (core:buffer-data read-buffer)
                                      :encoding encoding
                                      :start (core:buffer-start read-buffer)
                                      :end eol)))
            (core:buffer-skip read-buffer
                              (+ (- eol (core:buffer-start read-buffer))
                                 (length eol-octets)))
            (return (values line nil))))
        (let* ((read-size 4096)
               (position (core:buffer-reserve read-buffer read-size))
               (nb-read (read-io-stream stream
                                        (core:buffer-data read-buffer)
                                        position
                                        (+ position read-size))))
          (incf (core:buffer-end read-buffer) nb-read)
          (when (zerop nb-read)
            (let ((line
                    (text:decode-string (core:buffer-data read-buffer)
                                        :encoding encoding
                                        :start (core:buffer-start read-buffer)
                                        :end (core:buffer-end read-buffer))))
              (core:buffer-reset read-buffer)
              (return (values line t)))))))))

(defmethod streams:stream-clear-input ((stream io-stream))
  (with-slots (read-buffer) stream
    (core:buffer-reset read-buffer)))

(defmethod streams:stream-write-char ((stream io-stream) character)
  (declare (type character character))
  (with-slots (write-buffer external-format) stream
    (let* ((encoding (text:external-format-encoding external-format))
           (nb-octets
             (text:encoded-character-length character :encoding encoding))
           (position (core:buffer-reserve write-buffer nb-octets)))
      (text:encode-string (string character)
                          :encoding encoding
                          :octets (core:buffer-data write-buffer)
                          :offset position)
      (incf (core:buffer-end write-buffer) nb-octets)))
  character)

(defmethod streams:stream-line-column ((stream io-stream))
  (declare (ignore stream))
  nil)

(defmethod streams:stream-start-line-p ((stream io-stream))
  (eql (streams:stream-line-column stream) 0))

(defmethod streams:stream-write-string ((stream io-stream) string
                                        &optional (start 0) end)
  (declare (type string string))
  (when (> (length string) 0)
    (with-slots (write-buffer external-format) stream
      (let* ((end (or end (length string)))
             (encoding (text:external-format-encoding external-format))
             (nb-octets
               (text:encoded-string-length string :encoding encoding
                                                  :start start :end end))
             (position (core:buffer-reserve write-buffer nb-octets)))
        (text:encode-string string :encoding encoding
                                   :start start :end end
                                   :octets (core:buffer-data write-buffer)
                                   :offset position)
        (incf (core:buffer-end write-buffer) nb-octets))))
  string)

(defmethod streams:stream-terpri ((stream io-stream))
  (with-slots (external-format) stream
    (let ((eol-style (text:external-format-eol-style external-format)))
      (streams:stream-write-sequence stream (text:eol-octets eol-style))))
  nil)

(defmethod streams:stream-fresh-line ((stream io-stream))
  (unless (streams:stream-start-line-p stream)
    (streams:stream-terpri stream)
    t))

(defmethod streams:stream-advance-to-column ((stream io-stream) column)
  (declare (type (integer 0) column))
  (let ((current-column (streams:stream-line-column stream)))
    (when current-column
      (let ((nb-spaces (- column current-column)))
        (dotimes (i nb-spaces)
          (streams:stream-write-char stream #\Space)))
      t)))

(defun io-stream-read-more (stream)
  "Read more data from STREAM and store them in the read buffer. Return both the
number of octets read and the position of the first octet read in the read
buffer.

Note that the number of octets read will be zero if end-of-file was reached."
  (declare (type io-stream stream))
  (with-slots (read-buffer) stream
    (let* ((read-size 4096)
           (position (core:buffer-reserve read-buffer read-size))
           (nb-read (read-io-stream stream
                                    (core:buffer-data read-buffer)
                                    position
                                    (+ position read-size))))
      (incf (core:buffer-end read-buffer) nb-read)
      (values nb-read position))))
