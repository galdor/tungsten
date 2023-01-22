(in-package :postgresql)

(define-condition protocol-error (simple-error)
  ())

(defun protocol-error (format &rest arguments)
  (error 'protocol-error :format-control format :format-arguments arguments))

(defun check-message-size (data offset expected-size)
  (when (> (+ offset expected-size) (length data))
    (protocol-error "Truncated message body: expected ~D octets available ~
                     from offset ~D, but only ~D are available."
                    expected-size offset (- (length data) offset))))

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
      (case message-type
        (#\R
         (decode-message/authentication body))
        (t
         (protocol-error "Unhandled message type ~S." message-type))))))

(defun decode-message/authentication (data)
  (declare (type core:octet-vector data))
  (check-message-size data 0 4)
  (let ((type (core:binref :int32be data)))
    (case type
      (0
       '(:authentication-ok))
      (1
       '(:authentication-kerberos-v5))
      (2
       '(:authentication-cleartext-password))
      (5
       (check-message-size data 4 4)
       (let ((salt (core:make-octet-vector 4)))
         (replace salt data :start2 1)
         `(:authentication-md5-password ,salt)))
      (6
       '(:authentication-scm-credential))
      (7
       '(:authentication-gss))
      (9
       '(:authentication-sspi))
      (10
       (let ((mechanisms (decode-string-list data 1)))
         `(:authentication-sasl ,mechanisms)))
      (t
       (protocol-error "Unknown authentication type ~D." type)))))

(defun decode-string-list (data offset)
  (do ((strings nil)
       (i offset))
      ((or (>= i (length data))
           (= (aref data offset) 0))
       (when (>= i (length data))
         (protocol-error "Truncated string list."))
       (values (nreverse strings) (- i offset)))
    (multiple-value-bind (string length)
        (decode-string data i)
      (push string strings)
      (incf i length))))

(defun decode-string (data offset)
  (let ((zero (position 0 data :start offset)))
    (unless zero
      (protocol-error "Truncated string."))
    (values (text:decode-string data :start offset :end zero)
            (- zero offset))))

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
    (let ((size-offset (if message-type 1 0)))
      (setf (core:binref :int32be data size-offset) (fill-pointer data)))
    (write-sequence data stream)))

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
