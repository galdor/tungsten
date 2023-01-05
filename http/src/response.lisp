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

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (with-slots (status reason) response
      (format stream "~D ~S" status reason))))

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

(defun response-redirection-location (response)
  (declare (type response response))
  (with-slots (status header) response
    (case status
      ((301 302 303 307 308)
       (let ((location (header-field header "Location")))
         (when location
           (handler-case
               (uri:parse location)
             (uri:uri-parse-error ()
               (error 'invalid-redirection-location :location location))))))
      (t
       nil))))
