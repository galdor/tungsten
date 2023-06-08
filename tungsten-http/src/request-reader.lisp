(in-package :http)

(deftype request-reader-state ()
  '(member request-line header body trailer))

(defclass request-reader ()
  ((request
    :type request
    :reader request-reader-request)
   (header-length
    :type (integer 0)
    :initform 0)
   (body-representation)
   (state
    :type request-reader-state
    :initform 'request-line
    :accessor request-reader-state)))

(defun make-request-reader ()
  (let ((reader (make-instance 'request-reader)))
    (setf (slot-value reader 'request) (make-instance 'request))
    reader))

(defun read-request (reader stream)
  (declare (type request-reader reader)
           (type system:input-io-stream stream))
  (with-slots (request body-representation state) reader
    (loop
      (ecase state
        (request-line
         (multiple-value-bind (method target version)
             (read-request-line stream)
           (setf (request-method request) method
                 (request-target request) target
                 (request-version request) version))
         (setf state 'header))
        (header
         (when (read-request-header reader stream)
           (setf body-representation
                 (body-representation (request-header request)))
           (if body-representation
               (setf state 'body)
               (return-from read-request request))))
        (body
         (when (read-request-body reader stream)
           (if (eq (car body-representation) 'chunked)
               (setf state 'trailer)
               (return-from read-request request))))
        (trailer
         (unless (read-request-trailer reader stream)
           (return-from read-request request)))))))

(defun read-request-line (stream)
  (declare (type system:input-io-stream stream))
  (let* ((buffer (system:io-stream-read-buffer stream))
         (eol-octets (text:eol-octets :crlf))
         (eol (loop
                (when (zerop (system:io-stream-read-more stream))
                  (error 'connection-closed))
                (let ((eol (search eol-octets (core:buffer-data buffer)
                                   :start2 (core:buffer-start buffer)
                                   :end2 (core:buffer-end buffer))))
                  (cond
                    ((and (null eol)
                          (>= (core:buffer-length buffer)
                              *max-request-line-length*))
                     (error 'request-line-too-long
                            :data (core:buffer-content buffer)))
                    (eol
                     (return eol))))))
         (data (core:buffer-data buffer))
         (line-start (core:buffer-start buffer))
         (start line-start))
    (let (method target version end)
      ;; Method
      (setf end (or (position #.(char-code #\Space) data :start start :end eol)
                    (http-parse-error "truncated request method")))
      (setf method (parse-request-method data start end))
      (setf start (1+ end))
      ;; Target
      (setf end (or (position #.(char-code #\Space) data :start start :end eol)
                    (http-parse-error "truncated request target")))
      (let ((string (text:decode-string data :encoding :ascii
                                             :start start :end end)))
        (setf target (uri:parse string)))
      (setf start (1+ end))
      ;; Version
      (setf version (parse-protocol-version data start eol))
      ;; End
      (core:buffer-skip-to buffer (+ eol (length eol-octets)))
      (values method target version))))

(defun read-request-header (reader stream)
  (declare (type request-reader reader)
           (type system:input-io-stream stream))
  (with-slots (request header-length) reader
    (with-slots (header) request
      (do ((header-end nil))
          (header-end
           (core:buffer-skip (system:io-stream-read-buffer stream) 2) ; CRLF
           (setf header (nreverse header)))
        (let ((field-data
                (read-header-field stream :header-length header-length)))
          (cond
            ((null field-data)
             (setf header-end t))
            ((eq (car field-data) 'field)
             (push (cdr field-data) header))
            ((eq (car field-data) 'continuation)
             (add-header-field-continuation header (cdr field-data)))))))))

(defun read-request-trailer (reader stream)
  (declare (type request-reader reader)
           (type system:input-io-stream stream))
  (with-slots (request) reader
    (with-slots (trailer) request
      (do ((trailer-end nil))
          (trailer-end
           (core:buffer-skip (system:io-stream-read-buffer stream) 2) ; CRLF
           (setf trailer (nreverse trailer)))
        (let ((field-data (read-header-field stream)))
          (cond
            ((null field-data)
             (setf trailer-end t))
            ((eq (car field-data) 'field)
             (push (cdr field-data) trailer))
            ((eq (car field-data) 'continuation)
             (add-header-field-continuation trailer (cdr field-data)))))))))

(defun read-request-body (reader stream)
  (declare (type request-reader reader)
           (type system:input-io-stream stream))
  (with-slots (request body-representation) reader
    (with-slots (body) request
      (ecase (car body-representation)
        (plain
         (let ((content-length (cdr body-representation)))
           (unless body
             (setf body (make-array content-length :element-type 'core:octet)))
           (let ((nb-read (read-sequence body stream)))
             (when (< nb-read content-length)
               (error 'connection-closed))
             t)))
        (chunked
         (unless body
           (setf body (make-array 0 :element-type 'core:octet)))
         (do ((body-length 0)
              (chunk-length nil))
             ((eql chunk-length 0)
              t)
           (multiple-value-setq (body chunk-length)
             (read-chunk stream body))
           (incf body-length chunk-length)))))))
