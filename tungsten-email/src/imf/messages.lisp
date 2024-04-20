(in-package :imf)

(defvar *field-encoding-functions* (make-hash-table :test #'equal))

(deftype header ()
  'list)

(defun field (header name)
  (declare (type header header)
           (type string name))
  (cdr (assoc name header :test #'string=)))

(defun fields (header name)
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

(defun message-field (message name)
  (declare (type message message)
           (type string name))
  (field (message-header message) name))

(defun message-fields (message name)
  (declare (type message message)
           (type string name))
  (fields (message-header message) name))

(defun write-message (message stream &key (max-line-length 78) smtp)
  (declare (type message message)
           (type stream stream)
           (type (or (integer 1) null) max-line-length)
           (type boolean smtp))
  (with-encoder (stream :max-line-length max-line-length :smtp smtp)
    (with-slots (header body) message
      (dolist (field header)
        (encode-field (car field) (cdr field)))
      (when body
        (encode-eol)
        (etypecase body
          (string
           (mime:encode-quoted-printable body stream
                                         :max-line-length max-line-length
                                         :smtp smtp))
          (core:octet-vector
           (do* ((text (text:encode-base64 body))
                 (i 0)
                 (end (length text)))
                ((>= i end))
             (let ((line-end (min (+ i max-line-length -2) end)))
               (write-string text stream :start i :end line-end)
               (write-string (text:eol-string :crlf) stream)
               (setf i line-end)))))))))

(defun encode-field (name value)
  ;; In RFC 5322, all standard header fields are defined with the name
  ;; of the field and the colon character in the same token, so we must
  ;; not split lines between them.
  (encode-string (concatenate 'string (capitalize-header-name name) ": "))
  (let ((encoding-function (gethash name *field-encoding-functions*)))
    (if encoding-function
        (funcall encoding-function value)
        (encode-unstructured value)))
  (encode-eol))

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

(defun register-field-encoding-function (name function)
  (declare (type string name)
           (type (or symbol function) function))
  (setf (gethash name *field-encoding-functions*) function))

(defun unregister-field-encoding-function (name)
  (declare (type string name))
  (remhash name *field-encoding-functions*))

(defmacro define-field-encoding-function (name (value) &body body)
  `(register-field-encoding-function ,name (lambda (,value) ,@body)))

(define-field-encoding-function "Return-Path"
    (value)
  (declare (type (or mailbox null) value))
  (encode-character #\<)
  (when value
    (with-slots (local-part domain) value
      (encode-specific-address local-part domain)))
  (encode-character #\>))

(define-field-encoding-function "Resent-Date"
    (value)
  (declare (type time:datetime value))
  (encode-datetime value))

(define-field-encoding-function "Resent-From"
    (value)
  ;; We support addresses and not just mailboxes (RFC 6854)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Resent-Sender"
    (value)
  ;; We support addresses and not just mailboxes (RFC 6854)
  (declare (type address value))
  (encode-address value))

(define-field-encoding-function "Resent-To"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Resent-Cc"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Resent-Bcc"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Resent-Message-ID"
    (value)
  (declare (type message-id value))
  (encode-message-id value))

(define-field-encoding-function "Date"
    (value)
  (declare (type time:datetime value))
  (encode-datetime value))

(define-field-encoding-function "From"
    (value)
  ;; We support addresses and not just mailboxes (RFC 6854)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Sender"
    (value)
  ;; We support addresses and not just mailboxes (RFC 6854)
  (declare (type address value))
  (encode-address value))

(define-field-encoding-function "Reply-To"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "To"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Cc"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Bcc"
    (value)
  (declare (type (or address list) value))
  (if (listp value)
      (encode-addresses value)
      (encode-address value)))

(define-field-encoding-function "Message-ID"
    (value)
  (declare (type message-id value))
  (encode-message-id value))

(define-field-encoding-function "In-Reply-To"
    (value)
  (declare (type (or message-id list) value))
  (if (listp value)
      (encode-message-ids value)
      (encode-message-id value)))

(define-field-encoding-function "References"
    (value)
  (declare (type (or message-id list) value))
  (if (listp value)
      (encode-message-ids value)
      (encode-message-id value)))

(define-field-encoding-function "Subject"
    (value)
  (declare (type string value))
  (encode-unstructured value))

(define-field-encoding-function "Comments"
    (value)
  (declare (type string value))
  (encode-unstructured value))

(define-field-encoding-function "Keywords"
    (value)
  (declare (type (or list string) value))
  (if (listp value)
      (encode-phrases value)
      (encode-phrase value)))
