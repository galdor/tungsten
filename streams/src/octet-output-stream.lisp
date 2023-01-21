(in-package :streams)

(defclass octet-output-stream (streams:fundamental-binary-output-stream
                               streams:fundamental-character-output-stream)
  ((data
    :type core:octet-vector
    :initform (core:make-octet-vector 0))
   (length
    :type (integer 0)
    :initform 0)
   (external-format
    :type text:external-format
    :initarg :external-format
    :initform text:*default-external-format*)))

(defun make-octet-output-stream (&key (external-format
                                       text:*default-external-format*))
  (make-instance 'octet-output-stream :external-format external-format))

(defun octet-output-stream-data (stream)
  (declare (type octet-output-stream stream))
  (with-slots (data length) stream
    (setf data (adjust-array data length))))

(defmacro with-output-to-octet-vector ((stream &key external-format)
                                       &body body)
  `(let ((,stream (make-octet-output-stream
                   ,@(when external-format
                       `(:external-format ,external-format)))))
     ,@body
     (octet-output-stream-data stream)))

(defun octet-output-stream-reserve (stream size)
  (declare (type octet-output-stream stream)
           (type (integer 1) size))
  (with-slots (data length) stream
    (let ((capacity (length data))
          (new-length (+ length size)))
      (when (> new-length capacity)
        (let ((new-capacity (max new-length (* capacity 2))))
          (setf data (adjust-array data new-capacity))))
      length)))

(defmethod stream-clear-output ((stream octet-output-stream))
  (with-slots (length) stream
    (setf length 0))
  nil)

(defmethod stream-write-byte ((stream octet-output-stream) octet)
  (declare (type core:octet octet))
  (with-slots (data length) stream
    (let ((position (octet-output-stream-reserve stream 1)))
      (setf (aref data position) octet))
    (incf length))
  octet)

(defmethod stream-write-sequence ((stream octet-output-stream) octets
                                  &optional (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (let* ((end (or end (length octets)))
         (nb-octets (- end start)))
    (when (> nb-octets 0)
      (with-slots (data length) stream
        (let ((position (octet-output-stream-reserve stream nb-octets)))
          (replace data octets :start1 position :start2 start :end2 end)
          (incf length nb-octets)))))
  octets)

(defmethod stream-write-char ((stream octet-output-stream) character)
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

(defmethod streams:stream-line-column ((stream octet-output-stream))
  (declare (ignore stream))
  nil)

(defmethod streams:stream-start-line-p ((stream octet-output-stream))
  (eql (stream-line-column stream) 0))

(defmethod stream-write-string ((stream octet-output-stream) string
                                &optional (start 0) end)
  (declare (type string string))
  (let ((end (or end (length string)))
        (nb-characters (- end start)))
    (when (> nb-characters 0)
      (with-slots (data length external-format) stream
        (let* ((encoding (text:external-format-encoding external-format))
               (nb-octets
                 (text:encoded-string-length string :encoding encoding
                                                    :start start :end end))
               (position (octet-output-stream-reserve stream nb-octets)))
          (text:encode-string string :encoding encoding
                                     :start start :end end
                                     :octets data :offset position)
          (incf length nb-octets)))))
  string)

(defmethod stream-terpri ((stream octet-output-stream))
  (with-slots (external-format) stream
    (let ((eol-style (text:external-format-eol-style external-format)))
      (stream-write-sequence stream (text:eol-octets eol-style))))
  nil)

(defmethod stream-fresh-line ((stream octet-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defmethod stream-advance-to-column ((stream octet-output-stream) column)
  (declare (type (integer 0) column))
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (let ((nb-spaces (- column current-column)))
        (dotimes (i nb-spaces)
          (stream-write-char stream #\Space)))
      t)))
