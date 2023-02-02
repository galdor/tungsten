(in-package :text)

(defvar *encodings* (make-hash-table :test #'eq))

(defparameter *default-encoding* :utf-8)

(define-condition unencodable-character (error)
  ((character
    :type character
    :initarg :character)
   (encoding
    :type symbol
    :initarg :encoding))
  (:report
   (lambda (condition stream)
     (with-slots (character encoding) condition
       (format stream "Character ~S cannot be represented in encoding ~S."
               character encoding)))))

(define-condition decoding-error (error)
  ((octets
    :type core:octet-vector
    :initarg :octets)
   (offset
    :type index
    :initarg :offset)))

(define-condition invalid-octet (decoding-error)
  ((octet
    :type core:octet
    :initarg :octet)
   (encoding
    :type symbol
    :initarg :encoding))
  (:report
   (lambda (condition stream)
     (with-slots (octet encoding) condition
       (format stream "Octet ~S does not represent a character in encoding ~S."
               octet encoding)))))

(deftype encoded-character-length-function ()
  '(function (character) vector-length))

(deftype encoded-string-length-function ()
  '(function (string index index) vector-length))

(deftype character-encoding-function ()
  '(function (character vector index) vector-length))

(deftype decoded-string-length-function ()
  '(function (octet-vector index index) vector-length))

(deftype character-decoding-function ()
  '(function (octet-vector index index) (values character vector-length)))

(deftype string-decoding-function ()
  '(function (octet-vector index index string index) string))

(defclass encoding ()
  ((name
    :type string
    :initarg :name
    :reader encoding-name)
   (encoded-character-length-function
    :type encoded-character-length-function
    :initarg :encoded-character-length-function
    :reader encoding-encoded-character-length-function)
   (encoded-string-length-function
    :type encoded-string-length-function
    :initarg :encoded-string-length-function
    :reader encoding-encoded-string-length-function)
   (character-encoding-function
    :type character-encoding-function
    :initarg :character-encoding-function
    :reader encoding-character-encoding-function)
   (decoded-string-length-function
    :type decoded-string-length-function
    :initarg :decoded-string-length-function
    :reader encoding-decoded-string-length-function)
   (character-decoding-function
    :type character-decoding-function
    :initarg :character-decoding-function
    :reader encoding-character-decoding-function)))

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
