(in-package :ffi)

(defun base-type-p (type-name)
  (member type-name '(:void
                      :char :unsigned-char
                      :short :unsigned-short
                      :int :unsigned-int
                      :long :unsigned-long
                      :long-long :unsigned-long-long
                      :int8 :uint8
                      :int16 :uint16
                      :int32 :uint32
                      :int64 :uint64
                      :float :double
                      :pointer)))

(defvar *foreign-types* (make-hash-table :test #'eq))

(defclass foreign-type ()
  ((name
     :type symbol
     :initarg :name
     :reader foreign-type-name)
   (base-type
    :type symbol
    :initarg :base-type
    :reader foreign-type-base-type)
   (encoder
    :type symbol
    :initarg :encoder
    :accessor foreign-type-encoder)
   (decoder
    :type symbol
    :initarg :decoder
    :accessor foreign-type-decoder)))

(define-condition unknown-foreign-type (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-foreign-type-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown foreign type ~S." name)))))

(defun register-foreign-type (type)
  (setf (gethash (foreign-type-name type) *foreign-types*) type))

(defun foreign-type (name)
  (or (gethash name *foreign-types*)
      (error 'unknown-foreign-type :name name)))

(defun foreign-base-type (name)
  (if (base-type-p name)
      name
      (foreign-type-base-type (foreign-type name))))

;;;
;;; Type aliases
;;;

(defmacro define-foreign-type-alias (name base-type)
  (let ((type (gensym "TYPE-")))
    `(let ((,type (make-instance 'foreign-type
                                 :name ,name :base-type ,base-type)))
       (register-foreign-type ,type))))

;;;
;;; Enums
;;;

(defclass enum (foreign-type)
  ((constants
    :type list
    :initarg :constants
    :accessor enum-constants))
  (:default-initargs
   :base-type :int
   :encoder 'encode-enum-value
   :decoder 'decode-enum-value))

(defun encode-enum-value (enum constant-or-value)
  (declare (type (or symbol integer) constant-or-value))
  (etypecase constant-or-value
    (symbol
     (with-slots (name constants) enum
       (or (cdr (assoc constant-or-value constants))
           (error "unknown constant ~S for enum ~S"
                  constant-or-value name))))
    (integer
     constant-or-value)))

(defun decode-enum-value (enum value)
  (declare (type integer value))
  (with-slots (name constants) enum
    (or (car (find value constants :key #'cdr))
        value)))

(defmacro define-enum ((name &key (base-type :int)) (&rest constants))
  (let ((constant-pairs
          (mapcar
           (lambda (def)
             (destructuring-bind (constant value) def
               (cons constant value)))
           constants)))
    `(register-foreign-type
      (make-instance 'enum :name ',name
                           :base-type ,base-type
                           :constants ',constant-pairs))))
