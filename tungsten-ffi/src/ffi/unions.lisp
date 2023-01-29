(in-package :ffi)

(defclass foreign-union (foreign-type)
  ((members
    :type list
    :initarg :members
    :reader foreign-union-members)))

(defclass foreign-union-member ()
  ((name
    :type symbol
    :initarg :name
    :reader foreign-union-member-name)
   (type
    :type symbol
    :initarg :type
    :reader foreign-union-member-type)
   (count
    :type (integer 1)
    :initarg :count
    :initform 1
    :reader foreign-union-member-count)))

(define-condition unknown-foreign-union-member (error)
  ((foreign-union
    :type foreign-union
    :initarg :foreign-union)
   (name
    :type symbol
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name foreign-union) condition
       (format stream "Unknown foreign-union member ~S in ~A."
               name foreign-union)))))

(defmethod print-object ((member foreign-union-member) stream)
  (print-unreadable-object (member stream :type t)
    (princ (foreign-union-member-name member) stream)))

(defmethod initialize-instance :after ((foreign-union foreign-union)
                                       &key &allow-other-keys)
  (let ((max-size 0)
        (max-alignment 0))
    (dolist (member (foreign-union-members foreign-union))
      (with-slots (name type count) member
        (setf max-size (max max-size (* (foreign-type-size type) count)))
        (setf max-alignment
              (max max-alignment (foreign-type-alignment type)))))
    (unless (slot-boundp foreign-union 'size)
      (setf (slot-value foreign-union 'size) max-size))
    (unless (slot-boundp foreign-union 'alignment)
      (setf (slot-value foreign-union 'alignment) max-alignment))
    (setf (slot-value foreign-union 'base-type) :pointer)))

(defun find-foreign-union-member (name foreign-union)
  (or (find name (foreign-union-members foreign-union)
            :key #'foreign-union-member-name)
      (error 'unknown-foreign-union-member :foreign-union foreign-union
                                           :name name)))

(defmacro define-foreign-union ((name &key size) (&rest members-data))
  (let* ((members
           (mapcar
            (lambda (member-data)
              (destructuring-bind (name type &key (count 1))
                  member-data
                `(make-instance 'foreign-union-member
                                :name ,name
                                :type ,type
                                :count ,count)))
            members-data)))
    `(register-foreign-type
      (make-instance 'foreign-union
                     :name ',name
                     ,@(when size `(:size ,size))
                     :members (list ,@members)))))

(defun foreign-union-member (%pointer type-name member-name
                             &optional (offset 0))
  (let* ((type (foreign-type type-name))
         (member (find-foreign-union-member member-name type)))
    (foreign-value %pointer (foreign-union-member-type member) offset)))

(define-compiler-macro foreign-union-member (&whole form
                                             %pointer type-name member-name
                                             &optional (offset 0))
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-foreign-union-member member-name type))
            (member-type (foreign-union-member-type member)))
       `(foreign-value ,%pointer
                       ,(if (keywordp member-type) member-type `',member-type)
                       ,offset)))
    (t
     form)))

(defun write-foreign-union-member (%pointer type-name member-name offset value)
  (let* ((type (foreign-type type-name))
         (member (find-foreign-union-member member-name type)))
    (setf (foreign-value %pointer (foreign-union-member-type member) offset)
          value)))

(define-compiler-macro write-foreign-union-member (&whole form
                                            %pointer type-name member-name offset
                                            value)
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-foreign-union-member member-name type))
            (member-type (foreign-union-member-type member)))
       `(setf (foreign-value ,%pointer
                             ,(if (keywordp member-type)
                                  member-type
                                  `',member-type)
                             ,offset)
              ,value)))
    (t
     form)))

(defsetf foreign-union-member (%pointer type-name member-name
                               &optional (offset 0))
    (value)
  `(write-foreign-union-member
     ,%pointer ,type-name ,member-name ,offset ,value))

(defmacro with-foreign-union-members ((bindings %pointer type-name) &body body)
  `(symbol-macrolet
       (,@(mapcar
            (lambda (binding)
              (destructuring-bind (var member-name &key offset) binding
                `(,var (foreign-union-member ,%pointer ,type-name ,member-name
                                             ,@(when offset
                                                 `(:offset ,offset))))))
            bindings))
     ,@body))
