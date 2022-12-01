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

(define-condition unknown-foreign-type (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-foreign-type-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown foreign type ~S." name)))))

(defclass foreign-type ()
  ((name
     :type symbol
     :initarg :name
     :reader foreign-type-name)
   (base-type
    :type symbol
    :initarg :base-type
    :reader foreign-type-base-type)
   (size
    :type integer)
   (alignment
    :type integer)
   (encoder
    :type symbol
    :initarg :encoder
    :reader foreign-type-encoder)
   (decoder
    :type symbol
    :initarg :decoder
    :reader foreign-type-decoder)))

(defmethod initialize-instance :after ((type foreign-type) &key)
  (let ((base-type (foreign-type-base-type type)))
    (setf (slot-value type 'size) (%foreign-type-size base-type)
          (slot-value type 'alignment) (%foreign-type-alignment base-type))))

(defun register-foreign-type (type)
  (setf (gethash (foreign-type-name type) *foreign-types*) type))

(defun foreign-type (type-or-name)
  (etypecase type-or-name
    (foreign-type
     type-or-name)
    (t
     (or (gethash type-or-name *foreign-types*)
         (error 'unknown-foreign-type :name type-or-name)))))

(defun foreign-base-type (name)
  (if (base-type-p name)
      name
      (foreign-type-base-type (foreign-type name))))

(defmacro foreign-type-size (type)
  (cond
    ((base-type-p type)
     (%foreign-type-size type))
    (t
     (let ((type-var (gensym "TYPE-")))
       `(let ((,type-var ,type))
          (if (base-type-p ,type-var)
              (%foreign-type-size ,type-var)
              (slot-value (foreign-type ,type-var) 'size)))))))

(defmacro foreign-type-alignment (type)
  (cond
    ((base-type-p type)
     (%foreign-type-alignment type))
    (t
     (let ((type-var (gensym "TYPE-")))
       `(let ((,type-var ,type))
          (if (base-type-p ,type-var)
              (%foreign-type-alignment ,type-var)
              (slot-value (foreign-type ,type-var) 'alignment)))))))

(defmacro define-type-alias (name original-type)
  (let ((base-type (gensym "BASE-TYPE-")))
    `(let ((,base-type (foreign-base-type ,original-type)))
       (register-foreign-type
        (make-instance 'foreign-type
                       :name ',name :base-type ,base-type)))))
