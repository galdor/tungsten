(in-package :http)

(defclass response ()
  ((status
    :type response-status
    :initarg :status
    :accessor response-status)
   (reason
    :type (or string null)
    :initarg :reason
    :initform nil
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
    :accessor response-body)
   (trailer
    :type header
    :initarg :trailer
    :initform nil
    :accessor response-trailer)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type t)
    (with-slots (status reason) response
      (format stream "~D ~S" status reason))))

(defun make-response (status &key header body)
  (declare (type response-status status)
           (type header header)
           (type (or body null) body))
  (make-instance 'response :status status
                           :header header
                           :body body))

(defun make-plain-text-response (status body &key header)
  (declare (type response-status status)
           (type string body)
           (type header header))
  (unless (char= (char body (1- (length body))) #\Newline)
    (setf body (concatenate 'string body (string #\Newline))))
  (make-response status
                 :header (cons (cons "Content-Type" "text/plain") header)
                 :body body))

(defun make-error-response (status error)
  (declare (type response-status status)
           (type error error))
  (make-plain-text-response status (princ-to-string error)))

(defun response-header-field (response name)
  (declare (type response response)
           (type string name))
  (header-field (response-header response) name))

(defun (setf response-header-field) (value response name)
  (declare (type response response)
           (type string name)
           (type header-field-value value))
  (with-slots (header) response
    (let ((pair (assoc name header :test #'equalp)))
      (if pair
          (rplacd pair value)
          (push (cons name value) header))
      value)))

(defun add-response-header-field (response name value)
  (declare (type response response)
           (type string name)
           (type header-field-value value))
  (push (cons name value) (response-header response)))

(defun add-new-response-header-field (response name value)
  (declare (type response response)
           (type string name)
           (type header-field-value value))
  (with-slots (header) response
    (let ((pair (assoc name header :test #'equalp)))
      (unless pair
        (push (cons name value) header))
      value)))

(defun write-response (response stream)
  (declare (type response response)
           (type streams:fundamental-binary-output-stream stream)
           (type streams:fundamental-character-output-stream stream))
  (with-slots (status reason version header body) response
    (format stream "~A ~D ~A~%"
            (protocol-version-string version)
            status
            reason)
    ;; Header
    (dolist (field header)
      (let ((name (car field))
            (value (cdr field)))
        (format stream "~A: ~A~%" name value)))
    (terpri stream)
    (finish-output stream)
    ;; Body
    (etypecase body
      (null
       nil)
      (core:octet-vector
       (write-sequence body stream))
      (string
       (write-string body stream)))
    (finish-output stream)))

(defun read-response (stream &aux (response (make-instance 'response)))
  (declare (type system:input-io-stream stream))
  (multiple-value-bind (version status reason)
      (read-status-line stream)
    (setf (response-status response) status
          (response-reason response) reason
          (response-version response) version))
  (let ((header (read-header stream)))
    (setf (response-header response) header)
    (multiple-value-bind (body trailer)
        (read-body-and-trailer header stream)
      (setf (response-body response) body
            (response-trailer response) trailer)))
  response)

(defun read-status-line (stream)
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

(defun finalize-response (response)
  (declare (type response response))
  (mapc (lambda (function) (funcall function response))
        '(finalize-response/reason
          finalize-response/date
          finalize-response/body
          finalize-response/content-length)))

(defun finalize-response/reason (response)
  (declare (type response response))
  (with-slots (status reason) response
    (unless reason
      (setf reason (response-status-reason status)))))

(defun finalize-response/date (response)
  (declare (type response response))
  (let ((date (time:format-datetime (time:current-datetime) :rfc7231)))
    (add-new-response-header-field response "Date" date)))

(defun finalize-response/body (response)
  (declare (type response response))
  (with-slots (body) response
    (when body
      (etypecase body
        (core:octet-vector
         nil)
        (string
         (setf body (text:encode-string body)))))))

(defun finalize-response/content-length (response)
  (declare (type response response))
  ;; We assume that FINALIZE-RESPONSE/BODY has already been called
  (with-slots (body) response
    (let ((body-length (if body (length body) 0)))
      (add-new-response-header-field response "Content-Length"
                                     (princ-to-string body-length)))))
