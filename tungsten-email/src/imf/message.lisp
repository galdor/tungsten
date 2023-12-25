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

(defmethod write-tokens ((message message))
  (with-slots (header body) message
    (with-slots (stream max-line-length smtp) *line-writer*
      ;; Header
      (dolist (field header)
        (let ((name (car field))
              (value (cdr field)))
          (write-token (capitalize-header-name name))
          ;; In RFC 5322, all standard header fields are defined with the name
          ;; of the field and the colon character in the same token, so we must
          ;; not split lines between them.
          (write-token #\: :no-folding t)
          (write-token :space)
          (etypecase value
            (string
             ;; TODO word splitting
             (write-token value))
            ((or mailbox group)
             (write-tokens value)))
          (write-token :eol)))
      ;; Body
      (when body
        (write-token :eol)
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
