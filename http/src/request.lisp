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
    :accessor request-body)))

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
