(in-package :http)

(defclass response ()
  ((status
    :type response-status
    :initarg :status
    :accessor response-status)
   (reason
    :type string
    :initarg :reason
    :accessor response-reason)
   (version
    :type protocol-version
    :initarg :version
    :initform :http-1.1
    :accessor response-version)
   (header
    :type header
    :initarg :header
    :initform nil
    :accessor response-header)
   (body
    :type (or body null)
    :initarg :body
    :initform nil
    :accessor response-body)))

(defun read-response (stream &aux (response (make-instance 'response)))
  (declare (type system:io-stream stream))
  (multiple-value-bind (version status reason)
      (read-status-line stream)
    (setf (response-status response) status
          (response-reason response) reason
          (response-version response) version))
  (setf (response-header response) (read-header stream))
  (setf (response-body response) (read-body (response-header response) stream))
  response)

(defun read-status-line (stream)
  (declare (type system:io-stream stream))
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
                             *max-status-line-length*))
                    (error 'status-line-too-long
                           :data (core:buffer-content buffer)))
                   (eol
                    (return eol))))))
         (data (core:buffer-data buffer))
         (line-start (core:buffer-start buffer))
         (start line-start))
    (let (version status reason end)
      ;; Version
      (setf end (or (position #.(char-code #\Space) data :start start :end eol)
                    (http-parse-error "truncated protocol version")))
      (setf version (parse-protocol-version data start end))
      (setf start (1+ end))
      ;; Status
      (setf end (or (position #.(char-code #\Space) data :start start :end eol)
                    (http-parse-error "truncated response status")))
      (setf status (parse-response-status data start end))
      (setf start (1+ end))
      ;; Reason
      (setf reason (parse-response-reason data start eol))
      ;; End
      (core:buffer-skip-to buffer (+ eol (length eol-octets)))
      (values version status reason))))

(defun read-header (stream)
  (declare (type system:io-stream stream))
  (do ((buffer (system:io-stream-read-buffer stream))
       (eol-octets (text:eol-octets :crlf))
       (eol nil)
       (header nil))
      ((and eol (= eol (core:buffer-start buffer)))
       (nreverse header))
    (when (>= (core:buffer-length buffer) *max-header-length*)
      (error 'header-too-large :data (core:buffer-content buffer)))
    (setf eol (search eol-octets (core:buffer-data buffer)
                      :start2 (core:buffer-start buffer)
                      :end2 (core:buffer-end buffer)))
    (cond
      ((and eol (> eol (core:buffer-start buffer)))
       (cond
         ((htab-octet-p
           (aref (core:buffer-data buffer) (core:buffer-start buffer)))
          ;; Header field continuation
          (when (null header)
            (http-parse-error "invalid header field continuation"))
          (let ((start (position-if-not 'htab-octet-p (core:buffer-data buffer)
                                        :start (core:buffer-start buffer)
                                        :end eol)))
            (rplacd (car header)
                    (concatenate 'vector (cdar header)
                                 (core:octet-vector* #.(char-code #\Space))
                                 (subseq (core:buffer-data buffer)
                                         start eol)))))
         (t
          ;; Header field
          (multiple-value-bind (name value)
              (parse-header-field (core:buffer-data buffer)
                                  (core:buffer-start buffer) eol)
            (push (cons name value) header))))
       (core:buffer-skip-to buffer (+ eol (length eol-octets))))
      ((null eol)
       (when (zerop (system:io-stream-read-more stream))
         (error 'connection-closed))))))

(defun read-body (header stream)
  (declare (type header header)
           (type system:io-stream stream))
  (let* ((content-length-string (header-field header "Content-Length"))
         (content-length
           (progn
             (unless content-length-string
               (http-parse-error "missing Content-Length header field"))
             (handler-case
                 (parse-integer content-length-string)
               (error ()
                 (http-parse-error "invalid Content-Length header field"))))))
    (when (< content-length 0)
      (http-parse-error "invalid negative Content-Length header field"))
    (let* ((body (make-array content-length :element-type 'core:octet))
           (nb-read (read-sequence body stream)))
      (when (< nb-read content-length)
        (http-parse-error "truncated body"))
      body)))
