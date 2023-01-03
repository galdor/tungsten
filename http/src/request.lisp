(in-package :http)

(defclass request ()
  ((method
     :type request-method
     :initarg :method
     :initform :get
     :accessor request-method)
   (target
    :type request-target
    :initarg :target
    :initform (uri:make-uri :path "/")
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
    :accessor request-body)))

(defun make-request (method target &key (version :http-1.1) header body)
  (declare (type request-method method)
           (type request-target target)
           (type header header)
           (type (or body null) body))
  (make-instance 'request :method method
                          :target target
                          :version version
                          :header header
                          :body body))

(defun request-header-field (request name)
  (declare (type request request)
           (type header-field-name name))
  (header-field (request-header request) name))

(defun (setf request-header-field) (value request name)
  (declare (type request request)
           (type header-field-name name)
           (type header-field-value value))
  (with-slots (header) request
    (let* ((name-string (header-field-name-string name))
           (pair (assoc name-string header :key 'header-field-name-string
                                           :test #'equalp)))
      (if pair
          (rplacd pair value)
          (push (cons name value) header))
      value)))

(defun add-request-header-field (request name value)
  (declare (type request request)
           (type header-field-name name)
           (type header-field-value value))
  (push (cons name value) (request-header request)))

(defun add-new-request-header-field (request name value)
  (declare (type request request)
           (type header-field-name name)
           (type header-field-value value))
  (with-slots (header) request
    (let* ((name-string (header-field-name-string name))
           (pair (assoc name-string header :key 'header-field-name-string
                                           :test #'equalp)))
      (unless pair
        (push (cons name value) header))
      value)))

(defun request-connection-key (request)
  (declare (type request request))
  (let* ((target (request-target-uri (request-target request)))
         (scheme (or (uri:uri-scheme target) "http"))
         (host (restart-case
                   (let ((host (uri:uri-host target)))
                     (or host
                         (error 'missing-request-target-host :target target)))
                 (set-host (host)
                   :report "Set the host to connect to."
                   :interactive (lambda () (core:prompt-eval "Host string: "))
                   host)))
         (port (let ((port (uri:uri-port target)))
                 (cond
                   (port port)
                   ((equalp scheme "http") 80)
                   ((equalp scheme "https") 443))))
         (tls (equalp scheme "https")))
    (list host port tls)))

(defun write-request (request stream)
  (declare (type request request)
           (type streams:fundamental-binary-output-stream stream)
           (type streams:fundamental-character-output-stream stream))
  (with-slots (method target version header body) request
    ;; Request line
    (format stream "~A ~A ~A~%"
            (request-method-string method)
            (request-target-string target)
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
