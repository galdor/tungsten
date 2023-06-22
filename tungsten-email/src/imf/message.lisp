(in-package :imf)

(deftype header ()
  'list)

(deftype body ()
  'string)

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

(defun serialize-message (message &key (stream *standard-output*))
  (declare (type message message)
           (type stream stream))
  ;; See RFC 5322 2.1.1. Line Length Limits.
  (with-line-writer (stream :max-line-length 78)
    (with-slots (header body) message
      (write-header-tokens header)
      (when body
        (write-token :eol)
        ;; TODO Encoding
        (write-token body)))))

(defun write-header-tokens (header)
  (declare (type header header))
  (dolist (field header)
    (write-header-field-tokens (car field) (cdr field))))

(defun write-header-field-tokens (name value)
  (declare (type (or symbol string) name)
           (type (or string address) value)) ; TODO address lists?
  (etypecase name
    (symbol
     (write-token (capitalize-header-name (string-downcase name))))
    (string
     (write-token (capitalize-header-name name))))
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
