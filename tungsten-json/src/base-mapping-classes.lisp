(in-package :json)

(register-mapping-class :boolean 'boolean-mapping)
(register-mapping-class :number 'number-mapping)
(register-mapping-class :integer 'integer-mapping)
(register-mapping-class :string 'string-mapping)
(register-mapping-class :array 'array-mapping)
(register-mapping-class :object 'object-mapping)

(defclass boolean-mapping (mapping)
  ((value
    :type (or (member :true :false) null)
    :initarg :value
    :initform nil))
  (:default-initargs
   :base-types '(:boolean)))

(defmethod validate-value (value (mapping boolean-mapping))
  (with-slots (mapping-value) mapping
    (when (and mapping-value (not (eq mapping-value value)))
      (add-mapping-error value "boolean must be ~A" mapping-value))
    value))

(defmethod generate-value (value (mapping boolean-mapping))
  (declare (ignore mapping))
  value)

(defclass number-mapping (mapping)
  ((min
    :type (or float null)
    :initarg :min
    :initform nil)
   (max
    :type (or float null)
    :initarg :max
    :initform nil))
  (:default-initargs
   :base-types '(:number)))

(defmethod validate-value (value (mapping number-mapping))
  (with-slots (min max) mapping
    (when (and min (< value min))
      (add-mapping-error value "number must be greater than ~F" min))
    (when (and max (> value max))
      (add-mapping-error value "number must be lower than ~F" max))
    value))

(defmethod generate-value (value (mapping number-mapping))
  (declare (ignore mapping))
  value)

(defclass integer-mapping (mapping)
  ((min
    :type (or integer null)
    :initarg :min
    :initform nil)
   (max
    :type (or integer null)
    :initarg :max
    :initform nil))
  (:default-initargs
   :base-types '(:integer)))

(defmethod validate-value (value (mapping integer-mapping))
  (with-slots (min max) mapping
    (when (and min (< value min))
      (add-mapping-error value "integer must be greater than ~D" min))
    (when (and max (> value max))
      (add-mapping-error value "integer must be lower than ~D" max))
    value))

(defmethod generate-value (value (mapping integer-mapping))
  (declare (ignore mapping))
  value)

(defclass string-mapping (mapping)
  ((min-length
    :type (or (integer 0) null)
    :initarg :min-length
    :initform nil)
   (max-length
    :type (or (integer 0) null)
    :initarg :max-length
    :initform nil))
  (:default-initargs
   :base-types '(:string)))

(defmethod validate-value (value (mapping string-mapping))
  (with-slots (min-length max-length) mapping
    (when (and min-length (< (length value) min-length))
      (add-mapping-error value "string must contain more than ~D bytes"
                         min-length))
    (when (and max-length (> (length value) max-length))
      (add-mapping-error value "string must contain less than ~D bytes"
                         max-length))
    value))

(defmethod generate-value (value (mapping string-mapping))
  (declare (ignore mapping))
  value)

(defclass array-mapping (mapping)
  ((min-length
    :type (or (integer 0) null)
    :initarg :min-length
    :initform nil)
   (max-length
    :type (or (integer 0) null)
    :initarg :max-length
    :initform nil)
   (element
    :type (or symbol list)
    :initarg :element
    :initform nil))
  (:default-initargs
   :base-types '(:array)))

(defmethod validate-value (value (mapping array-mapping))
  (let ((nb-elements (length value)))
    (with-slots (min-length max-length element) mapping
      (when (and min-length (< nb-elements min-length))
        (add-mapping-error value "array must contain more than ~D elements"
                           min-length))
      (when (and max-length (> nb-elements max-length))
        (add-mapping-error value "array must contain less than ~D elements"
                           max-length))
      (when element
        (dotimes (i nb-elements)
          (setf (aref value i) (validate-child i (aref value i) element))))))
  value)

(defmethod generate-value (value (mapping array-mapping))
  (let ((nb-elements (length value)))
    (with-slots (element) mapping
      (when element
        (dotimes (i nb-elements)
          (setf (aref value i) (generate-child i (aref value i) element))))))
  value)

(defclass object-mapping (mapping)
  ((class
    :type (or symbol null)
    :initarg :class
    :initform nil
    :accessor object-mapping-class)
   (members
    :type list
    :initarg :members
    :initform nil
    :accessor object-mapping-members)
   (required
    :type list
    :initarg :required
    :initform nil
    :accessor object-mapping-required))
  (:default-initargs
   :base-types '(:object)))

(defmethod validate-value (value (mapping object-mapping))
  (with-slots (class members required) mapping
    (let ((output-value (when class (make-instance class)))
          (missing-members nil))
      (dolist (name required)
        (unless (assoc name value :test #'string=)
          (push name missing-members)))
      (when missing-members
        (add-mapping-error value "object must contain the following ~
                                    member~P: ~{~S~^, ~}"
                           (length missing-members)
                           (nreverse missing-members)))
      (dolist (member members)
        (destructuring-bind (name slot member-mapping) member
          (let ((member-value (assoc name value :test #'string=)))
            (when member-value
              (let ((decoded-value
                      (validate-child name (cdr member-value) member-mapping)))
                (if class
                    (setf (slot-value output-value slot) decoded-value)
                    (push (cons slot decoded-value) output-value)))))))
      (if class
          output-value
          (nreverse output-value)))))

(defmethod generate-value (value (mapping object-mapping))
  (let ((output-value nil))
    (with-slots (members) mapping
      (dolist (member members)
        (destructuring-bind (name slot member-mapping) member
          (cond
            ((listp value)
             (let ((member-value (assoc name value :test #'string=)))
               (when member-value
                 (let ((encoded-value
                         (generate-child name member-value member-mapping)))
                   (push (cons name encoded-value) output-value)))))
            (t
             (let* ((member-value (when (slot-boundp value slot)
                                    (slot-value value slot)))
                    (encoded-value
                      (generate-child name member-value member-mapping)))
               (push (cons name encoded-value) output-value)))))))
    (nreverse output-value)))
