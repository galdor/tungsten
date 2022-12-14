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
    :initarg :name))
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
    :type integer
    :initarg :size)
   (alignment
    :type integer
    :initarg :alignment)
   (encoder
    :type (or symbol null)
    :initarg :encoder
    :initform nil
    :reader foreign-type-encoder)
   (decoder
    :type (or symbol null)
    :initarg :decoder
    :initform nil
    :reader foreign-type-decoder)))

(defmethod initialize-instance :after ((type foreign-type)
                                       &key &allow-other-keys)
  ;; Some types such as structures do not set the base type with
  ;; :DEFAULT-INITARGS to avoid automatic size and alignment computation.
  (when (slot-boundp type 'base-type)
    (let ((base-type (foreign-type-base-type type)))
      (unless (slot-boundp type 'size)
        (setf (slot-value type 'size) (%foreign-type-size base-type)))
      (unless (slot-boundp type 'alignment)
        (setf (slot-value type 'alignment)
              (%foreign-type-alignment base-type))))))

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

(defmacro foreign-type-size (type-name)
  (cond
    ((constantp type-name)
     (if (base-type-p type-name)
         (%foreign-type-size type-name)
         (slot-value (foreign-type (cadr type-name)) 'size)))
    (t
     (let ((type-var (gensym "TYPE-")))
       `(let ((,type-var ,type-name))
          (if (base-type-p ,type-var)
              (%foreign-type-size ,type-var)
              (slot-value (foreign-type ,type-var) 'size)))))))

(defmacro foreign-type-alignment (type-name)
  (cond
    ((constantp type-name)
     (if (base-type-p type-name)
         (%foreign-type-alignment type-name)
         (slot-value (foreign-type (cadr type-name)) 'alignment)))
    (t
     (let ((type-var (gensym "TYPE-")))
       `(let ((,type-var ,type-name))
          (if (base-type-p ,type-var)
              (%foreign-type-alignment ,type-var)
              (slot-value (foreign-type ,type-var) 'alignment)))))))

(defun encode-foreign-value (value type-name)
  (if (base-type-p type-name)
      value
      (let* ((type (foreign-type type-name))
             (encoder (foreign-type-encoder type)))
        (if encoder
            (funcall encoder type value)
            value))))

(defun decode-foreign-value (value type-name)
  (if (base-type-p type-name)
      value
      (let* ((type (foreign-type type-name))
             (decoder (foreign-type-decoder type)))
        (if decoder
            (funcall decoder type value)
            value))))

(defmacro define-type-alias (name original-type)
  (let ((base-type (gensym "BASE-TYPE-")))
    `(let ((,base-type (foreign-base-type ,original-type)))
       (register-foreign-type
        (make-instance 'foreign-type
                       :name ',name :base-type ,base-type)))))
