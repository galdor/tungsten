(in-package :ffi)

(defclass foreign-bitset (foreign-type)
  ((constants
    :type list
    :initarg :constants
    :reader foreign-bitset-constants))
  (:default-initargs
   :base-type :int
   :encoder 'encode-foreign-bitset-value
   :decoder 'decode-foreign-bitset-value))

(defun encode-foreign-bitset-value (bitset integer-or-values)
  (declare (type (or (integer 0) list) integer-or-values))
  (etypecase integer-or-values
    (integer
     integer-or-values)
    (list
     (with-slots (name constants) bitset
       (flet ((encode-value (constant-or-value)
                (etypecase constant-or-value
                  (symbol
                   (or (cdr (assoc constant-or-value constants))
                       (error "unknown constant ~S for foreign bitset ~S"
                              constant-or-value name)))
                  (integer
                   constant-or-value))))
         (apply #'logior (mapcar #'encode-value integer-or-values)))))))

(defun decode-foreign-bitset-value (bitset value)
  (declare (type integer value))
  (with-slots (name constants) bitset
    (let ((values nil))
      (dolist (constant constants values)
        (unless (zerop (logand value (cdr constant)))
          (push (car constant) values))))))

(defmacro define-foreign-bitset ((name &key (base-type :int))
                                 (&rest constants))
  (let ((constant-pairs
          (mapcar
           (lambda (def)
             (destructuring-bind (constant value) def
               (cons constant value)))
           constants)))
    `(register-foreign-type
      (make-instance 'foreign-bitset
                     :name ',name
                     :base-type (foreign-base-type ,base-type)
                     :constants ',constant-pairs))))
