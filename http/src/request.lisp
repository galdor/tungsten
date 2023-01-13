(in-package :http)

(defclass request ()
  ((method
     :type request-method
     :initarg :method
     :accessor request-method)
   (target
    :type uri:uri
    :initarg :target
    :accessor request-target)
   (version
    :type protocol-version
    :initarg :version
    :initform :http-1.1
    :accessor request-version)
   (header
    :type header
    :initarg :header
    :initform nil
    :accessor request-header)
   (body
    :type (or body null)
    :initarg :body
    :initform nil
    :accessor request-body)
   (trailer
    :type header
    :initarg :trailer
    :initform nil
    :accessor request-trailer)))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type t)
    (with-slots (method target) request
      (format stream "~A ~S" method (uri:serialize target)))))

(defun request-header-field (request name)
  (declare (type request request)
           (type string name))
  (header-field (request-header request) name))

(defun (setf request-header-field) (value request name)
  (declare (type request request)
           (type string name)
           (type header-field-value value))
  (with-slots (header) request
    (let ((pair (assoc name header :test #'equalp)))
      (if pair
          (rplacd pair value)
          (push (cons name value) header))
      value)))

(defun add-request-header-field (request name value)
  (declare (type request request)
           (type string name)
           (type header-field-value value))
  (push (cons name value) (request-header request)))

(defun add-new-request-header-field (request name value)
  (declare (type request request)
           (type string name)
           (type header-field-value value))
  (with-slots (header) request
    (let ((pair (assoc name header :test #'equalp)))
      (unless pair
        (push (cons name value) header))
      value)))

(defun request-keep-connection-alive-p (request)
  (declare (type request request))
  (let ((version (request-version request))
        (connection (request-header-field request "Connection")))
    (or (equalp connection "keep-alive")
        (and (null connection) (eq version :http-1.1)))))

(defun write-request (request stream)
  (declare (type request request)
           (type streams:fundamental-binary-output-stream stream)
           (type streams:fundamental-character-output-stream stream))
  (with-slots (method target version header body) request
    ;; Request line
    (format stream "~A ~A ~A~%"
            (request-method-string method)
            (uri:serialize target)
            (protocol-version-string version))
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

(defun read-request (stream &aux (request (make-instance 'request)))
  (declare (type system:io-stream stream))
  (multiple-value-bind (method target version)
      (read-request-line stream)
    (setf (request-method request) method
          (request-target request) target
          (request-version request) version))
  (let ((header (read-header stream)))
    (setf (request-header request) header)
    (multiple-value-bind (body trailer)
        (read-body-and-trailer header stream)
      (setf (request-body request) body
            (request-trailer request) trailer)))
  request)

(defun read-request-line (stream)
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
