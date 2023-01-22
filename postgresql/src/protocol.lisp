(in-package :postgresql)

(define-condition protocol-error (simple-error)
  ())

(defun protocol-error (format &rest arguments)
  (error 'protocol-error :format-control format :format-arguments arguments))

(defstruct (decoder
            (:copier nil)
            (:predicate nil))
  (data nil :type (or core:octet-vector null))
  (start 0 :type (integer 0))
  (end 0 :type (integer 0)))

(defun decoder-eof-p (decoder)
  (declare (type decoder decoder))
  (with-slots (start end) decoder
    (>= start end)))

(defun decoder-starts-with (decoder prefix)
  (declare (type decoder decoder)
           (type core:octet prefix))
  (with-slots (data start end) decoder
    (and (< start end)
         (= (aref data start) prefix))))

(defun decoder-skip (decoder n)
  (declare (type decoder decoder)
           (type (integer 1) n))
  (incf (decoder-start decoder) n))

(defun decode-int8 (decoder)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (when (>= start end)
      (protocol-error "Truncated 1 byte integer."))
    (prog1 (core:binref :int8 data start)
      (incf start))))

(defun decode-int32 (decoder)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (when (> (+ start 4) end)
      (protocol-error "Truncated 4 byte integer."))
    (prog1 (core:binref :int32be data start)
      (incf start 4))))

(defun decode-string (decoder)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (let ((zero (position 0 data :start start :end end)))
      (unless zero
        (protocol-error "Truncated string."))
      (prog1 (text:decode-string data :start start :end zero)
        (setf start (1+ zero))))))

(defun decode-string-list (decoder)
  (declare (type decoder decoder))
  (do ((strings nil))
      ((decoder-eof-p decoder)
       (protocol-error "Truncated string list."))
    (when (decoder-starts-with decoder 0)
      (decoder-skip decoder 1)
      (return (nreverse strings)))
    (push (decode-string decoder) strings)))

(defun decode-octets (decoder nb-octets)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (when (> (+ start nb-octets) end)
      (protocol-error "Truncated octet sequence."))
    (let ((octets (core:make-octet-vector nb-octets)))
      (replace octets data :start2 start)
      (incf start nb-octets)
      octets)))

(defun decode-error-and-notice-fields (decoder)
  (declare (type decoder decoder))
  (do ((fields nil))
      ((decoder-eof-p decoder)
       (protocol-error "Truncated field list."))
    (when (decoder-starts-with decoder 0)
      (decoder-skip decoder 1)
      (return (nreverse fields)))
    (let* ((type (code-char (decode-int8 decoder)))
           (value (decode-string decoder))
           (field (case type
                    (#\S (cons :l10n-severity value))
                    (#\V (cons :severity value))
                    (#\C (cons :code value))
                    (#\M (cons :message value))
                    (#\D (cons :detail value))
                    (#\H (cons :hint value))
                    (#\P (cons :position value))
                    (#\p (cons :internal-position value))
                    (#\q (cons :internal-query value))
                    (#\W (cons :where value))
                    (#\s (cons :schema value))
                    (#\t (cons :table value))
                    (#\c (cons :column value))
                    (#\d (cons :data-type value))
                    (#\n (cons :constraint value))
                    (#\F (cons :file value))
                    (#\L (cons :internal-line value))
                    (#\R (cons :routine value))
                    (t (cons type value)))))
      (push field fields))))

(defun read-message (stream)
  (declare (type stream stream))
  (let ((header (core:make-octet-vector 5)))
    (unless (= (read-sequence header stream) 5)
      (error 'end-of-file :stream stream))
    (let* ((message-type (code-char (core:binref :int8 header 0)))
           (size (core:binref :int32be header 1))
           (body-size (- size 4))
           (body (core:make-octet-vector body-size)))
      (unless (= (read-sequence body stream) body-size)
        (error 'end-of-file :stream stream))
      (let ((decoder (make-decoder :data body :start 0 :end (length body))))
        (case message-type
          (#\E
           (decode-message/error-response decoder))
          (#\R
           (decode-message/authentication decoder))
          (t
           (protocol-error "Unhandled message type ~S." message-type)))))))

(defun decode-message/error-response (decoder)
  (declare (type decoder decoder))
  `(:error-response . ,(decode-error-and-notice-fields decoder)))

(defun decode-message/authentication (decoder)
  (declare (type decoder decoder))
  (let ((type (decode-int32 decoder)))
    (case type
      (0
       '(:authentication-ok))
      (1
       '(:authentication-kerberos-v5))
      (2
       '(:authentication-cleartext-password))
      (5
       (let ((salt (decode-octets decoder 4)))
         `(:authentication-md5-password ,salt)))
      (6
       '(:authentication-scm-credential))
      (7
       '(:authentication-gss))
      (9
       '(:authentication-sspi))
      (10
       (let ((mechanisms (decode-string-list decoder)))
         `(:authentication-sasl ,mechanisms)))
      (t
       (protocol-error "Unknown authentication type ~D." type)))))

(defun write-message (message-type value stream)
  (declare (type (or standard-char null) message-type)
           (type stream stream))
  (let* ((header-size (if message-type 5 4))
         (data (make-array 32 :element-type 'core:octet
                              :adjustable t :fill-pointer header-size)))
    (when message-type
      (setf (core:binref :int8 data 0) (char-code message-type)))
    (labels
        ((reserve (size)
           (let* ((capacity (length data))
                  (position (fill-pointer data))
                  (required-size (+ position size)))
             (when (> required-size capacity)
               (let ((new-capacity (max required-size (* capacity 2))))
                 (setf data (adjust-array data new-capacity))
                 (incf (fill-pointer data) size)
                 position))))
         (encode-value (value)
           (cond
             ((and (listp value) (listp (car value)))
              (mapc #'encode-value value))
             (t
              (let ((value-type (car value))
                    (value (cadr value)))
                (ecase value-type
                  (int8
                   (setf (core:binref :int8 data (reserve 1)) value))
                  (int16
                   (setf (core:binref :int16be data (reserve 2)) value))
                  (int32
                   (setf (core:binref :int32be data (reserve 4)) value))
                  (string
                   (let* ((nb-octets (text:encoded-string-length value))
                          (offset (reserve (1+ nb-octets))))
                     (text:encode-string value :octets data :offset offset
                                               :nb-octets nb-octets)
                     (setf (aref data (+ offset nb-octets)) 0)))
                  (octets
                   (let* ((value (cadr value))
                          (nb-octets (length value))
                          (offset (reserve nb-octets)))
                     (replace data value :start1 offset)))))))))
      (encode-value value))
    (let ((size-offset (if message-type 1 0))
          (size (length data)))
      (when message-type
        (decf size))
      (setf (core:binref :int32be data size-offset) size))
    (write-sequence data stream)
    (finish-output stream)))

(defun write-startup-message (major-version minor-version parameters stream)
  (declare (type (signed-byte 16) major-version minor-version)
           (type list parameters)
           (type stream stream))
  (write-message nil `((int16 ,major-version)
                       (int16 ,minor-version)
                       ,@(mapcar (lambda (parameter)
                                   `((string ,(car parameter))
                                     (string,(cdr parameter))))
                                 parameters)
                       (int8 0))
                 stream))

(defun write-password-message (password stream)
  (declare (type string password)
           (type stream stream))
  (write-message #\p `(string ,password) stream))

(defun compute-password-md5-hash (user password salt)
  (declare (type string user password)
           (type core:octet-vector salt))
  (let* ((key (concatenate 'string password user))
         (hashed-key (openssl:compute-hex-digest key :md5))
         (salted-key (core:octet-vector* (text:encode-string hashed-key) salt))
         (hashed-salted-key (openssl:compute-hex-digest salted-key :md5)))
    (concatenate 'string "md5" hashed-salted-key)))
