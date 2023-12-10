(in-package :imf)

(deftype header ()
  'list)

(defun header-field (header name)
  (declare (type header header)
           (type string name))
  (cdr (assoc name header :test #'string=)))

(defun header-fields (header name)
  (declare (type header header)
           (type string name))
  (let ((values nil))
    (dolist (field header (nreverse values))
      (when (string= (car field) name)
        (push (cdr field) values)))))

(deftype body ()
  '(or string core:octet-vector))

(defclass message ()
  ((header
    :type header
    :initarg :header
    :initform nil
    :accessor message-header)
   (body
    :type (or body null)
    :initarg :body
    :initform nil
    :accessor message-body)))

(defun make-message (header body)
  (declare (type header header)
           (type (or body null) body))
  (make-instance 'message :header header :body body))

(defun message-header-field (message name)
  (declare (type message message)
           (type string name))
  (header-field (message-header message) name))

(defun message-header-fields (message name)
  (declare (type message message)
           (type string name))
  (header-fields (message-header message) name))

(defun serialize-message (message &key (stream *standard-output*))
  (declare (type message message)
           (type stream stream))
  ;; See RFC 5322 2.1.1. Line Length Limits.
  (with-line-writer (stream :max-line-length 78)
    (with-slots (header body) message
      (write-header-tokens header)
      (when body
        (write-token :eol)
        (serialize-body body stream)))))

(defun write-header-tokens (header)
  (declare (type header header))
  (dolist (field header)
    (write-header-field-tokens (car field) (cdr field))))

(defun write-header-field-tokens (name value)
  (declare (type string name)
           (type (or string address) value)) ; TODO address lists?
  (write-token (capitalize-header-name name))
  ;; In RFC 5322, all standard header fields are defined with the name of the
  ;; field and the colon character in the same token, so we must not split
  ;; lines between them.
  (write-token #\: :no-folding t)
  (write-token :space)
  (etypecase value
    (string
     ;; TODO Word splitting
     (write-token value))
    (mailbox
     (write-mailbox-tokens value))
    (group
     (write-group-tokens value)))
  (write-token :eol))

(defun capitalize-header-name (name)
  (declare (type string name))
  (do ((capitalized-name (copy-seq name))
       (i 0 (let ((dash (position #\- name :start i)))
              (if dash
                  (1+ dash)
                  (length name)))))
      ((>= i (length name))
       capitalized-name)
    (setf (char capitalized-name i) (char-upcase (char name i)))
    (incf i)))

(defun serialize-body (body stream)
  (declare (type body body)
           (type stream stream))
  (etypecase body
    (string
     (mime:encode-quoted-printable body stream))
    (core:octet-vector
     (write-string (text:encode-base64 body) stream))))
