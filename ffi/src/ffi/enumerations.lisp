(in-package :ffi)

(defclass enum (foreign-type)
  ((constants
    :type list
    :initarg :constants
    :reader enum-constants))
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
