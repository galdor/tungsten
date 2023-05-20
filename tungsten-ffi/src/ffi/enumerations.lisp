(in-package :ffi)

(defclass foreign-enumeration (foreign-type)
  ((constants
    :type list
    :initarg :constants
    :reader enum-constants))
  (:default-initargs
   :base-type :int
   :encoder 'encode-foreign-enumeration-value
   :decoder 'decode-foreign-enumeration-value))

(defun encode-foreign-enumeration-value (enum constant-or-value)
  (declare (type (or symbol integer) constant-or-value))
  (etypecase constant-or-value
    (symbol
     (with-slots (name constants) enum
       (or (cdr (assoc constant-or-value constants))
           (error "unknown constant ~S for foreign enumeration ~S"
                  constant-or-value name))))
    (integer
     constant-or-value)))

(defun decode-foreign-enumeration-value (enum value)
  (declare (type integer value))
  (with-slots (name constants) enum
    (or (car (find value constants :key #'cdr))
        value)))

(defmacro define-foreign-enumeration ((name &key (base-type :int))
                                      (&rest constants))
  (let ((constant-pairs
          (mapcar
           (lambda (def)
             (destructuring-bind (constant value) def
               (cons constant value)))
           constants)))
    `(register-foreign-type
      (make-instance 'foreign-enumeration
                     :name ',name
                     :base-type (foreign-base-type ,base-type)
                     :constants ',constant-pairs))))
