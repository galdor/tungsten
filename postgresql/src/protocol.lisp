(in-package :postgresql)

(define-condition protocol-error (simple-error)
  ())

(defun protocol-error (format &rest arguments)
  (error 'protocol-error :format-control format :format-arguments arguments))

(define-condition backend-error (error)
  ((l10n-severity
    :type (or symbol string)
    :reader backend-error-l10n-severity)
   (severity
    :type (or symbol string)
    :reader backend-error-severity)
   (code
    :type string
    :reader backend-error-code)
   (message
    :type string
    :reader backend-error-message)
   (detail
    :type (or string null)
    :reader backend-error-detail)
   (position
    :type (or integer null)
    :reader backend-error-position)
   (internal-position
    :type (or integer null)
    :reader backend-error-internal-position)
   (internal-query
    :type (or string null)
    :reader backend-error-internal-query)
   (where
    :type (or string null)
    :reader backend-error-where)
   (schema
    :type (or string null)
    :reader backend-error-schema)
   (table
    :type (or string null)
    :reader backend-error-table)
   (column
    :type (or string null)
    :reader backend-error-column)
   (data-type
    :type (or string null)
    :reader backend-error-data-type)
   (constraint
    :type (or string null)
    :reader backend-error-constraint)
   (file
    :type (or string null)
    :reader backend-error-file)
   (line
    :type (or integer null)
    :reader backend-error-line)
   (routine
    :type (or string null)
    :reader backend-error-routine))
  (:report
   (lambda (condition stream)
     (with-slots (severity code message) condition
       (format stream "PostgreSQL backend error: ~A ~A ~A."
               severity code message)))))

(defun backend-error (fields)
  (declare (type list fields))
  (let ((error (make-condition 'backend-error)))
    (macrolet ((copy-field (name)
                 `(setf (slot-value error ',name)
                        (cdr (assoc ,(intern (symbol-name name) :keyword)
                                    fields)))))
      (copy-field l10n-severity)
      (copy-field severity)
      (copy-field code)
      (copy-field message)
      (copy-field detail)
      (copy-field position)
      (copy-field internal-position)
      (copy-field internal-query)
      (copy-field where)
      (copy-field schema)
      (copy-field table)
      (copy-field column)
      (copy-field data-type)
      (copy-field constraint)
      (copy-field file)
      (copy-field line)
      (copy-field routine))
    (error error)))

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

(defun decode-int16 (decoder)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (when (> (+ start 2) end)
      (protocol-error "Truncated 2 byte integer."))
    (prog1 (core:binref :int16be data start)
      (incf start 2))))

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

(defun decode-octet (decoder)
  (declare (type decoder decoder))
  (with-slots (data start end) decoder
    (when (>= start end)
      (protocol-error "Missing octet."))
    (prog1 (aref data 0)
      (incf start))))

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
                    (#\S (cons :l10n-severity (parse-severity value)))
                    (#\V (cons :severity (parse-severity value)))
                    (#\C (cons :code value))
                    (#\M (cons :message value))
                    (#\D (cons :detail value))
                    (#\H (cons :hint value))
                    (#\P (cons :position (parse-integer value)))
                    (#\p (cons :internal-position (parse-integer value)))
                    (#\q (cons :internal-query value))
                    (#\W (cons :where value))
                    (#\s (cons :schema value))
                    (#\t (cons :table value))
                    (#\c (cons :column value))
                    (#\d (cons :data-type value))
                    (#\n (cons :constraint value))
                    (#\F (cons :file value))
                    (#\L (cons :internal-line (parse-integer value)))
                    (#\R (cons :routine value))
                    (t (cons type value)))))
      (push field fields))))

(defun decode-row-description (decoder)
  (declare (type decoder decoder))
  (let* ((name (decode-string decoder))
         (table-oid (decode-int32 decoder))
         (column-attribute (decode-int16 decoder))
         (type-oid (decode-int32 decoder))
         (type-size (decode-int16 decoder))
         (type-modifier (decode-int32 decoder))
         (format-code (decode-int16 decoder))
         (format (case format-code
                   (0 :text)
                   (1 :binary)
                   (t (protocol-error "Unknown field format code ~D."
                                      format-code)))))
    (list (cons :name name)
          (cons :table-oid table-oid)
          (cons :column-attribute column-attribute)
          (cons :type-oid type-oid)
          (cons :type-size type-size)
          (cons :type-modifier type-modifier)
          (cons :format format))))

(defun parse-command-tag (string)
  (let ((space (position #\Space string)))
    (unless space
      (protocol-error "Invalid command tag ~S." string))
    (let ((name (cond ((string= string "INSERT" :end1 space) :insert)
                      ((string= string "DELETE" :end1 space) :delete)
                      ((string= string "UPDATE" :end1 space) :update)
                      ((string= string "SELECT" :end1 space) :select)
                      ((string= string "MOVE" :end1 space) :move)
                      ((string= string "FETCH" :end1 space) :fetch)
                      ((string= string "COPY" :end1 space) :copy)
                      (t (subseq string 0 space))))
          ;; Find the last space character to support "INSERT 0 <N>"
          (value (let ((start (1+ (position #\Space string :from-end t))))
                   (handler-case
                       (parse-integer string :start start)
                     (error ()
                       (protocol-error "Invalid command tag count ~S."
                                       (subseq string start)))))))
      (cons name value))))

(defun parse-severity (string)
  (declare (type string string))
  (cond
    ((member string '("LOG" "DEBUG" "INFO" "NOTICE" "WARNING" "ERROR"
                      "FATAL" "PANIC")
             :test #'string=)
     (intern string :keyword))
    (t
     string)))

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
      (let ((decoder (make-decoder :data body :start 0 :end (length body)))
            (decode (case message-type
                      (#\C 'decode-message/command-complete)
                      (#\D 'decode-message/data-row)
                      (#\E 'decode-message/error-response)
                      (#\K 'decode-message/backend-key-data)
                      (#\R 'decode-message/authentication)
                      (#\S 'decode-message/parameter-status)
                      (#\T 'decode-message/row-description)
                      (#\Z 'decode-message/ready-for-query)
                      (t
                       (protocol-error "Unhandled message type ~S."
                                       message-type)))))
        (funcall decode decoder)))))

(defun decode-message/command-complete (decoder)
  (declare (type decoder decoder))
  (let ((tag (decode-string decoder)))
    (list :command-complete (parse-command-tag tag))))

(defun decode-message/data-row (decoder)
  (declare (type decoder decoder))
  (let* ((nb-columns (decode-int16 decoder))
         (columns (make-array nb-columns)))
    (dotimes (i nb-columns (list :data-row columns))
      (let ((size (decode-int32 decoder)))
        (cond
          ((< size -1)
           (protocol-error "Invalid data row column size ~D." size))
          ((= size -1)
           (setf (aref columns i) :null))
          (t
           (setf (aref columns i) (decode-octets decoder size))))))))

(defun decode-message/error-response (decoder)
  (declare (type decoder decoder))
  (list :error-response (decode-error-and-notice-fields decoder)))

(defun decode-message/backend-key-data (decoder)
  (declare (type decoder decoder))
  (let* ((process-id (decode-int32 decoder))
         (secret-key (decode-int32 decoder)))
    (list :backend-key-data process-id secret-key)))

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
         (list :authentication-md5-password salt)))
      (6
       '(:authentication-scm-credential))
      (7
       '(:authentication-gss))
      (9
       '(:authentication-sspi))
      (10
       (let ((mechanisms (decode-string-list decoder)))
         (list :authentication-sasl mechanisms)))
      (11
       (let ((data (decode-octets decoder (- (decoder-end decoder)
                                             (decoder-start decoder)))))
         (list :authentication-sasl-continue
               (text:decode-string data :encoding :ascii))))
      (12
       (let ((data (decode-octets decoder (- (decoder-end decoder)
                                             (decoder-start decoder)))))
         (list :authentication-sasl-final
               (text:decode-string data :encoding :ascii))))
      (t
       (protocol-error "Unknown authentication type ~D." type)))))

(defun decode-message/parameter-status (decoder)
  (declare (type decoder decoder))
  (let* ((name (decode-string decoder))
         (value (decode-string decoder)))
    (list :parameter-status name value)))

(defun decode-message/row-description (decoder)
  (declare (type decoder decoder))
  (let ((nb-fields (decode-int16 decoder))
        (fields nil))
    (dotimes (i nb-fields (list :row-description (nreverse fields)))
      (push (decode-row-description decoder) fields))))

(defun decode-message/ready-for-query (decoder)
  (declare (type decoder decoder))
  (let* ((status-octet (decode-octet decoder))
         (status (case status-octet
                   (#.(char-code #\I) :idle)
                   (#.(char-code #\T) :in-transaction)
                   (#.(char-code #\E) :in-failed-transaction)
                   (t
                    (protocol-error
                     "Unknown backend transaction status ~S." status-octet)))))
    (list :ready-for-query status)))

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
                   (let* ((nb-octets (length value)))
                     (when (> nb-octets 0)
                       (let ((offset (reserve nb-octets)))
                         (replace data value :start1 offset)))))))))))
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

(defun write-sasl-initial-response-message (mechanism string stream)
  (declare (type string mechanism)
           (type (or string null) string)
           (type stream stream))
  (let ((data (when string
                (text:encode-string string :encoding :ascii))))
    (write-message #\p `((string ,mechanism)
                         (int32 ,(if data (length data) -1))
                         (octets ,data))
                   stream)))

(defun write-sasl-response-message (string stream)
  (declare (type string string)
           (type stream stream))
  (let ((data (text:encode-string string :encoding :ascii)))
    (write-message #\p `((octets ,data))
                   stream)))

(defun write-termination-message (stream)
  (declare (type stream stream))
  (write-message #\X nil stream))

(defun write-query-message (query stream)
  (declare (type string query)
           (type stream stream))
  (write-message #\Q `(string ,query) stream))

(defun compute-password-md5-hash (user password salt)
  (declare (type string user password)
           (type core:octet-vector salt))
  (let* ((key (concatenate 'string password user))
         (hashed-key (openssl:compute-hex-digest key :md5))
         (salted-key (core:octet-vector* (text:encode-string hashed-key) salt))
         (hashed-salted-key (openssl:compute-hex-digest salted-key :md5)))
    (concatenate 'string "md5" hashed-salted-key)))
