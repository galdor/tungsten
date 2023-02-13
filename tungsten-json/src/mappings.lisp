(in-package :json)

(defvar *mappings* (make-hash-table))

(defvar *mapping-pointer* nil)
(defvar *mapping-errors* nil)

(deftype base-type ()
  '(member :null :boolean :integer :number :string :array :object))

(defun value-base-type (value)
  (etypecase value
    ((member :null) :null)
    ((member :true :false) :boolean)
    (integer :integer)
    (number :number)
    (string :string)
    (vector :array)
    (list :object)))

(define-condition unknown-mapping (error)
  ((name
    :type symbol
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown JSON mapping ~S." name)))))

(defun register-mapping (name mapping)
  (setf (gethash name *mappings*) mapping))

(defmacro define-mapping (name class-name &rest arguments)
  `(register-mapping ',name (mapping '(,class-name ,@arguments))))

(defun delete-mapping (name)
  (remhash name *mappings*))

(defun find-mapping (name)
  (or (gethash name *mappings*)
      (error 'unknown-mapping :name name)))

(defun mapping (mapping)
  (declare (type (or symbol list) mapping))
  (etypecase mapping
    (symbol
     (find-mapping mapping))
    (list
     (let ((class (find-mapping-class (car mapping))))
       (apply #'make-instance class (cdr mapping))))))

(defclass mapping-error ()
  ((value
    :initarg :value
    :reader mapping-error-value)
   (pointer
    :type pointer
    :initarg :pointer
    :reader mapping-error-pointer)
   (description
    :type string
    :initarg :description
    :reader mapping-error-description)))

(defun add-mapping-error (value format &rest arguments)
  (push (make-instance 'mapping-error
                       :value value :pointer *mapping-pointer*
                       :description (apply #'format nil format arguments))
        *mapping-errors*))

(define-condition invalid-value (error)
  ((value
    :initarg :value
    :reader invalid-value-value)
   (mapping-errors
    :type list
    :initarg :mapping-errors
    :reader invalid-value-mapping-errors))
  (:report
   (lambda (condition stream)
     (format stream "Invalid JSON value:~%")
     (dolist (error (invalid-value-mapping-errors condition))
       (with-slots (pointer description) error
         (terpri stream)
         (if pointer
             (format stream "Pointer: ~A~%Error:   ~A~%"
                     (serialize-pointer pointer) description)
             (format stream "Error:   ~A~%" description)))))))

(defclass mapping ()
  ((base-types
    :type list
    :initarg :base-types
    :initform nil
    :accessor mapping-base-types)))

(defgeneric validate-value (value mapping))
(defgeneric generate-value (value mapping))

(defun validate-child (child-pointer value mapping)
  (declare (type child-pointer child-pointer)
           (type (or symbol list) mapping))
  (let ((*mapping-pointer* (child-pointer child-pointer *mapping-pointer*)))
    (validate-value value (mapping mapping))))

(defun generate-child (child-pointer value mapping)
  (declare (type child-pointer child-pointer)
           (type (or symbol list) mapping))
  (let ((*mapping-pointer* (child-pointer child-pointer *mapping-pointer*)))
    (generate-value value (mapping mapping))))

(defun validate (value mapping)
  (declare (type (or symbol list) mapping))
  (let ((mapping (mapping mapping))
        (*mapping-errors* nil)
        (*mapping-pointer* nil))
    (prog1 (validate-value value mapping)
      (unless (null *mapping-errors*)
        (error 'invalid-value :value value
                              :mapping-errors (nreverse *mapping-errors*))))))

(defun generate (value mapping)
  (declare (type (or symbol list) mapping))
  (let ((mapping (mapping mapping))
        (*mapping-errors* nil)
        (*mapping-pointer* nil))
    (prog1 (generate-value value mapping)
      (unless (null *mapping-errors*)
        (error 'invalid-value :value value
                              :mapping-errors (nreverse *mapping-errors*))))))

(defmethod validate-value :around (value (mapping mapping))
  (with-slots (base-types) mapping
    (when base-types
      (let ((value-base-type (value-base-type value)))
        (cond
          ((member value-base-type base-types)
           (call-next-method))
          (t
           ;; TODO It can be done with one single format string
           (case (length base-types)
             (1
              (add-mapping-error
               value "value is not of type ~A" (car base-types)))
             (2
              (add-mapping-error value "value is not of type ~A or ~A"
                                 (first base-types) (second base-types)))
             (t
              (add-mapping-error value "value is not of type ~{~A~^, ~} or ~A"
                                 (butlast base-types) (last base-types))))
           value))))))
