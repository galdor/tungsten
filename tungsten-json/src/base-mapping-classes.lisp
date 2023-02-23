(in-package :json)

(register-mapping-class :any 'any-mapping)
(register-mapping-class :null 'null-mapping)
(register-mapping-class :boolean 'boolean-mapping)
(register-mapping-class :number 'number-mapping)
(register-mapping-class :integer 'integer-mapping)
(register-mapping-class :string 'string-mapping)
(register-mapping-class :array 'array-mapping)
(register-mapping-class :object 'object-mapping)
(register-mapping-class :or 'or-mapping)

(defclass any-mapping (mapping)
  ())

(defmethod validate-value (value (mapping any-mapping))
  (declare (ignore mapping))
  value)

(defmethod generate-value (value (mapping any-mapping))
  (declare (ignore mapping))
  value)

(defclass null-mapping (mapping)
  ()
  (:default-initargs
   :base-types '(:null)))

(defmethod validate-value (value (mapping null-mapping))
  (declare (ignore mapping))
  (cond
    ((eq value :null)
     nil)
    (t
     (add-mapping-error value "value is not null")
     value)))

(defmethod generate-value (value (mapping null-mapping))
  (declare (ignore mapping))
  (cond
    (value
     (add-mapping-error value "value is not null")
     value)
    (t
     :null)))

(defclass boolean-mapping (mapping)
  ((value
    :type (or (member :true :false) null)
    :initarg :value
    :initform nil))
  (:default-initargs
   :base-types '(:boolean)))

(defmethod validate-value (value (mapping boolean-mapping))
  (with-slots ((mapping-value value)) mapping
    (when (and mapping-value (not (eq mapping-value value)))
      (add-mapping-error value "boolean must be ~A" mapping-value))
    (ecase value
      (:true t)
      (:false nil))))

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
   :base-types '(:number :integer)))

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
    :initform nil)
   (value
    :type list
    :initarg :value
    :initform nil))
  (:default-initargs
   :base-types '(:string)))

(defmethod validate-value (value (mapping string-mapping))
  (with-slots (min-length max-length (valid-values value)) mapping
    (when (and min-length (< (length value) min-length))
      (add-mapping-error value "string must contain more than ~D bytes"
                         min-length))
    (when (and max-length (> (length value) max-length))
      (add-mapping-error value "string must contain less than ~D bytes"
                         max-length))
    (cond
      (valid-values
       (flet ((predicate (valid-value)
                (etypecase valid-value
                  (string (string= value valid-value))
                  (list (string= value (car valid-value))))))
         (let ((valid-value (find-if #'predicate valid-values)))
           (cond
             (valid-value
              (etypecase valid-value
                (string valid-value)
                (list (cadr valid-value))))
             (t
              (add-mapping-error value
                                 "string must have one of the following ~
                                 values: ~{~S~^, ~}"
                                 valid-values)
              value)))))
      (t
       value))))

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
   (name
    :type (or symbol list)
    :initarg :name
    :initform nil)
   (value
    :type (or symbol list)
    :initarg :value
    :initform nil)
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
  (with-slots (class (name-mapping name) (value-mapping value)
               members required)
      mapping
    (let ((names (mapcar #'car members))
          (missing-members nil)
          (output-value (when class (make-instance class))))
      ;; Required members
      (dolist (name required)
        (unless (assoc name value :test #'string=)
          (push name missing-members)))
      (when missing-members
        (add-mapping-error value "object must contain the following ~
                                    member~P: ~{~S~^, ~}"
                           (length missing-members)
                           (nreverse missing-members)))
      ;; Defined members
      (dolist (member members)
        (destructuring-bind (name slot member-mapping) member
          (let ((member-value (assoc name value :test #'string=)))
            (when member-value
              (let ((decoded-value
                      (validate-child name (cdr member-value) member-mapping)))
                (if class
                    (setf (slot-value output-value slot) decoded-value)
                    (push (cons slot decoded-value) output-value)))))))
      ;; Other members
      (dolist (member-value value)
        (unless (member (car member-value) names :test #'string=)
          (let ((decoded-value
                  (if value-mapping
                      (validate-child (car member-value) (cdr member-value)
                                      value-mapping)
                      (cdr member-value))))
            (push (cons (car member-value) decoded-value) output-value))))
      ;; Member names
      (when name-mapping
        (dolist (member-value output-value)
          (rplaca member-value
                  (handler-case
                      (validate (car member-value)
                                name-mapping :pointer *mapping-pointer*)
                    (invalid-value (condition)
                      (dolist (mapping-error
                               (invalid-value-mapping-errors condition))
                        (add-mapping-error (car member-value)
                                           "invalid key ~S: ~A"
                                           (car member-value)
                                           (mapping-error-description
                                            mapping-error))))))))
      ;; Final value
      (if class
          output-value
          (nreverse output-value)))))

(defmethod generate-value (value (mapping object-mapping))
  (with-slots (class (value-mapping value) members) mapping
    (let ((names (mapcar #'first members))
          (slots (mapcar #'second members))
          (output-value nil))
      ;; Defined members
      (dolist (member members)
        (destructuring-bind (name slot member-mapping) member
          (cond
            ((listp value)
             (let ((member-value (find-if (lambda (value)
                                            (etypecase (car value)
                                              (symbol
                                               (eq (car value) slot))
                                              (string
                                               (string= (car value) name))))
                                          value)))
               (when member-value
                 (let ((encoded-value
                         (generate-child name (cdr member-value)
                                         member-mapping)))
                   (push (cons name encoded-value) output-value)))))
            (t
             (let* ((member-value (when (slot-boundp value slot)
                                    (slot-value value slot)))
                    (encoded-value
                      (generate-child name member-value member-mapping)))
               (push (cons name encoded-value) output-value))))))
      ;; Other members
      (unless class
        (dolist (member-value value)
          (let ((defined
                  (etypecase (car member-value)
                    (symbol
                     (member (car member-value) slots))
                    (string
                     (member (car member-value) names :test #'string=)))))
            (unless defined
              (let ((encoded-value
                      (if value-mapping
                          (generate-child (car member-value) (cdr member-value)
                                          value-mapping)
                          (cdr member-value))))
                (push (cons (car member-value) encoded-value) output-value))))))
      ;; Final value
      (nreverse output-value))))

(defclass or-mapping (mapping)
  ((mappings
     :type list
     :initarg :mappings
     :accessor or-mapping-mappings)))

(defmethod initialize-instance :after ((mapping or-mapping)
                                       &key &allow-other-keys)
  (let (base-types)
    (dolist (child-mapping (or-mapping-mappings mapping))
      (dolist (base-type (mapping-base-types (mapping child-mapping)))
        (push base-type base-types)))
    (setf (mapping-base-types mapping)
          (nreverse (delete-duplicates base-types)))))

(defmethod validate-value (value (mapping or-mapping))
  (let ((base-types (mapping-base-types mapping)))
    (unless (member (value-base-type value) base-types)
      (add-mapping-error value "value does not match any expected format")))
  (let ((mapping-errors nil))
    (dolist (child-mapping (or-mapping-mappings mapping))
      (handler-case
          (return-from validate-value (validate value child-mapping
                                                :pointer *mapping-pointer*))
        (invalid-value (condition)
          (when (member (value-base-type value)
                        (mapping-base-types (mapping child-mapping)))
            (dolist (mapping-error (invalid-value-mapping-errors condition))
              (push mapping-error mapping-errors))))))
    (setf *mapping-errors* (append mapping-errors *mapping-errors*))))

(defmethod generate-value (value (mapping or-mapping))
  (dolist (child-mapping (or-mapping-mappings mapping))
    (handler-case
        (return-from generate-value (generate value child-mapping
                                              :pointer *mapping-pointer*))
      (invalid-value ()
        nil))
    (add-mapping-error value "value does not match any expected format")))
