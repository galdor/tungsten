(in-package :postgresql)

(defvar *codecs* (make-hash-table :test #'equal))

(deftype oid ()
  '(unsigned-byte 32))

(define-condition unknown-codec (error)
  ((type
    :type (or symbol null)
    :initarg :type
    :initform nil
    :reader unknown-codec-type)
   (oid
    :type (or oid null)
    :initarg :oid
    :initform nil
    :reader unknown-codec-oid))
  (:report
   (lambda (condition stream)
     (with-slots (type oid) condition
       (if type
           (format stream "No codec found for type ~S." type)
           (format stream "No codec found for type OID ~D." oid))))))

(define-condition unencodable-value (error)
  ((value
    :initarg :value
    :reader unencodable-value-value))
  (:report
   (lambda (condition stream)
     (with-slots (value) condition
       (format stream "Value ~S cannot be encoded to a PostgreSQL value."
               value)))))

(define-condition value-decoding-error (simple-error)
  ((octets
    :type core:octet-vector
    :initarg :octets))
  (:report
   (lambda (condition stream)
     (format stream "Cannot decode value: ~?."
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))

(defun value-decoding-error (octets format &rest arguments)
  (error 'value-decoding-error :octets octets
                               :format-control format
                               :format-arguments arguments))

(defclass codec ()
  ((type
    :type symbol
    :initarg :type
    :reader codec-type)
   (oid
    :type oid
    :initarg :oid
    :reader codec-oid)
   (encoding-function
    :type (or symbol function)
    :initarg :encoding-function
    :reader codec-encoding-function)
   (decoding-function
    :type (or symbol function)
    :initarg :decoding-function
    :reader codec-decoding-function)))

(defmethod print-object ((codec codec) stream)
  (print-unreadable-object (codec stream :type t)
    (with-slots (type oid) codec
      (format stream "~A ~D" type oid))))

(defun find-codec (type-or-oid)
  (declare (type (or symbol oid) type-or-oid))
  (or (gethash type-or-oid *codecs*)
      (etypecase type-or-oid
        (oid (error 'unknown-codec :oid type-or-oid))
        (t (error 'unknown-codec :type type-or-oid)))))

(defun register-codec (type oid encoding-function decoding-function)
  (let ((codec (make-instance 'codec :type type
                                     :oid oid
                                     :encoding-function encoding-function
                                     :decoding-function decoding-function)))
    (delete-codec type)
    (delete-codec oid)
    (setf (gethash type *codecs*) codec
          (gethash oid *codecs*) codec)))

(defun delete-codec (type-or-oid)
  (let ((codec (gethash type-or-oid *codecs*)))
    (when codec
      (remhash (codec-type codec) *codecs*)
      (remhash (codec-oid codec) *codecs*)
      t)))

(defun encode-value (value)
  (cond
    ((null value)
     '(:null . nil))
    ((eq value :void)
     (encode-value '(:void . nil)))
    ((eq value :true)
     (encode-value '(:boolean . t)))
    ((eq value :false)
     (encode-value '(:boolean . nil)))
    ((typep value '(signed-byte 16))
     (encode-value `(:int2 . ,value)))
    ((typep value '(signed-byte 32))
     (encode-value `(:int4 . ,value)))
    ((typep value '(signed-byte 64))
     (encode-value `(:int8 . ,value)))
    ((typep value 'single-float)
     (encode-value `(:float4 . ,value)))
    ((typep value 'double-float)
     (encode-value `(:float8 . ,value)))
    ((typep value 'string)
     (encode-value `(:text . ,value)))
    ((typep value 'core:octet-vector)
     (encode-value `(:bytea . ,value)))
    ((consp value)
     (let* ((codec (find-codec (car value)))
            (value (cdr value))
            (octets (funcall (codec-encoding-function codec) value)))
       (cons (codec-oid codec) octets)))
    (t
     (error 'unencodable-value :value value))))

(defun decode-value (octets type-or-oid)
  (declare (type core:octet-vector octets)
           (type (or symbol oid) type-or-oid))
  (let ((codec (find-codec type-or-oid)))
    (funcall (codec-decoding-function codec) octets)))

;;;
;;; Booleans
;;;

(defun encode-value/boolean (value)
  (if value
      (core:octet-vector* 1)
      (core:octet-vector* 0)))

(defun decode-value/boolean (octets)
  (unless (= (length octets) 1)
    (value-decoding-error octets "boolean is not 1 byte long"))
  (case (aref octets 0)
    (0 :false)
    (t :true)))

(register-codec :boolean 16 'encode-value/boolean 'decode-value/boolean)

;;;
;;; Binary data
;;;

(defun encode-value/bytea (value)
  value)

(defun decode-value/bytea (octets)
  octets)

(register-codec :bytea 17 'encode-value/bytea 'decode-value/bytea)

;;;
;;; Integers
;;;

(defun integer-value-encoding-function (size type)
  (lambda (value)
    (let ((octets (core:make-octet-vector size)))
      (setf (core:binref type octets) value)
      octets)))

(defun integer-value-decoding-function (size type)
  (lambda (octets)
    (when (/= (length octets) size)
      (value-decoding-error octets "integer is not ~D byte long" size))
    (core:binref type octets)))

(register-codec :int8 20
                (integer-value-encoding-function 8 :int64be)
                (integer-value-decoding-function 8 :int64be))

(register-codec :int2 21
                (integer-value-encoding-function 2 :int16be)
                (integer-value-decoding-function 2 :int16be))

(register-codec :int4 23
                (integer-value-encoding-function 4 :int32be)
                (integer-value-decoding-function 4 :int32be))

(register-codec :oid 26
                (integer-value-encoding-function 4 :uint32be)
                (integer-value-decoding-function 4 :uint32be))
;;;
;;; Strings

(defun encode-value/text (value)
  (text:encode-string value))

(defun decode-value/text (octets)
  (text:decode-string octets))

(register-codec :name 19 'encode-value/text 'decode-value/text)
(register-codec :text 25 'encode-value/text 'decode-value/text)
(register-codec :bpchar 1042 'encode-value/text 'decode-value/text)
(register-codec :varchar 1043 'encode-value/text 'decode-value/text)
