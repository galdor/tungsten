(in-package :text)

(defvar *encodings* (make-hash-table :test #'eq))

(defparameter *default-encoding* :utf-8)

(deftype encoded-string-length-function ()
  '(function (simple-string index index) vector-length))

(deftype string-encoding-function ()
  '(function (simple-string index index vector index) octet-vector))

(deftype decoded-string-length-function ()
  '(function (octet-vector index index) vector-length))

(deftype string-decoding-function ()
  '(function (octet-vector index index string index) simple-string))

(defclass encoding ()
  ((name
    :type string
    :initarg :name
    :reader encoding-name)
   (encoded-string-length-function
    :type encoded-string-length-function
    :initarg :encoded-string-length-function
    :reader encoding-encoded-string-length-function)
   (encoding-function
    :type string-encoding-function
    :initarg :encoding-function
    :reader encoding-encoding-function)
   (decoded-string-length-function
    :type decoded-string-length-function
    :initarg :decoded-string-length-function
    :reader encoding-decoded-string-length-function)
   (decoding-function
    :type string-decoding-function
    :initarg :decoding-function
    :reader encoding-decoding-function)))

(defun encoding (id-or-encoding)
  (declare (type (or symbol encoding) id-or-encoding))
  (typecase id-or-encoding
    (symbol
     (or (gethash id-or-encoding *encodings*)
         (error "Unknown encoding ~A." id-or-encoding)))
    (encoding
     id-or-encoding)))

(defun register-encoding (id encoding)
  (declare (type symbol id)
           (type encoding encoding))
  (setf (gethash id *encodings*) encoding))

(defun unregister-encoding (id)
  (declare (type symbol id))
  (remhash id *encodings*))

(defmacro define-encoding (id () &rest args)
  `(register-encoding ,id (make-instance 'encoding ,@args)))
