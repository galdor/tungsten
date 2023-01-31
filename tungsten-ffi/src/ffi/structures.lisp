(in-package :ffi)

(defclass struct (foreign-type)
  ((members
    :type list
    :initarg :members
    :reader struct-members)))

(defclass struct-member ()
  ((name
    :type symbol
    :initarg :name
    :reader struct-member-name)
   (type
    :type symbol
    :initarg :type
    :reader struct-member-type)
   (count
    :type (integer 1)
    :initarg :count
    :initform 1
    :reader struct-member-count)
   (offset
    :type (integer 0)
    :initarg :offset
    :reader struct-member-offset)))

(define-condition unknown-struct-member (error)
  ((struct
    :type struct
    :initarg :struct)
   (name
    :type symbol
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name struct) condition
       (format stream "Unknown struct member ~S in ~A." name struct)))))

(defmethod print-object ((member struct-member) stream)
  (print-unreadable-object (member stream :type t)
    (princ (struct-member-name member) stream)))

(defmethod initialize-instance :after ((struct struct) &key &allow-other-keys)
  (let ((offset 0)
        (max-alignment 0))
    ;; Compute the offset of each member
    (dolist (member (struct-members struct))
      (with-slots (name type count) member
        (let ((alignment (foreign-type-alignment type)))
          (setf max-alignment (max max-alignment alignment))
          (cond
            ((slot-boundp member 'offset)
             (let ((member-offset (struct-member-offset member)))
               (when (< member-offset offset)
                 (error "invalid offset ~D for member ~S" member-offset name))
               (setf offset member-offset)))
            (t
             (unless (zerop (mod offset alignment))
               (incf offset
                     (- alignment (mod offset alignment))))
             (setf (slot-value member 'offset) offset))))
        (incf offset (* (foreign-type-size type) count))))
    ;; Compute the size of the structure; it must be a multiple of the
    ;; alignment of the structure itself, which is the largest member
    ;; alignment. This is true at least on x86_64.
    (let ((size offset))
      (unless (zerop (mod size max-alignment))
        (incf size (- max-alignment (mod size max-alignment))))
      (unless (slot-boundp struct 'size)
        (setf (slot-value struct 'size) size))
      (unless (slot-boundp struct 'alignment)
        (setf (slot-value struct 'alignment) max-alignment)))
    (setf (slot-value struct 'base-type) :pointer)))

(defun find-struct-member (name struct)
  (or (find name (struct-members struct) :key #'struct-member-name)
      (error 'unknown-struct-member :struct struct :name name)))

(defmacro define-struct ((name &key size) (&rest members-data))
  (let* ((members
           (mapcar
            (lambda (member-data)
              (destructuring-bind (name type &key (count 1) offset)
                  member-data
                `(make-instance 'struct-member
                                :name ,name
                                :type ,type
                                :count ,count
                                ,@(when offset (list :offset offset)))))
            members-data)))
    `(register-foreign-type
      (make-instance 'struct
                     :name ',name
                     ,@(when size `(:size ,size))
                     :members (list ,@members)))))

(defun struct-member (%pointer type-name member-name &optional (offset 0))
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (foreign-value (%pointer+ %pointer (struct-member-offset member))
                   (struct-member-type member)
                   offset)))

(define-compiler-macro struct-member (&whole form
                                      %pointer type-name member-name
                                      &optional (offset 0))
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-struct-member member-name type))
            (member-type (struct-member-type member)))
       `(foreign-value (%pointer+ ,%pointer ,(struct-member-offset member))
                       ,(if (keywordp member-type) member-type `',member-type)
                       ,offset)))
    (t
     form)))

(defun write-struct-member (%pointer type-name member-name offset value)
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (setf (foreign-value (%pointer+ %pointer (struct-member-offset member))
                         (struct-member-type member)
                         offset)
          value)))

(define-compiler-macro write-struct-member (&whole form
                                            %pointer type-name member-name offset
                                            value)
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-struct-member member-name type))
            (member-type (struct-member-type member)))
       `(setf (foreign-value (%pointer+ ,%pointer ,(struct-member-offset member))
                             ,(if (keywordp member-type)
                                  member-type
                                  `',member-type)
                             ,offset)
              ,value)))
    (t
     form)))

(defsetf struct-member (%pointer type-name member-name &optional (offset 0))
    (value)
  `(write-struct-member ,%pointer ,type-name ,member-name ,offset ,value))

(defun struct-member-pointer (%pointer type-name member-name &optional (offset 0))
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (%pointer+ %pointer (+ (struct-member-offset member)
                           (* offset (foreign-type-size
                                      (struct-member-type member)))))))

(define-compiler-macro struct-member-pointer (&whole form
                                              %pointer type-name member-name
                                              &optional (offset 0))
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-struct-member member-name type))
            (member-type (struct-member-type member)))
       `(%pointer+
         ,%pointer
         ,(if (constantp offset)
              (+ (struct-member-offset member)
                  (* offset (foreign-type-size member-type)))
              `(+ ,(struct-member-offset member)
                  (* ,offset ,(foreign-type-size member-type)))))))
    (t
     form)))

(defmacro with-struct-members ((bindings %pointer type-name) &body body)
  `(symbol-macrolet
       (,@(mapcar
            (lambda (binding)
              (destructuring-bind (var member-name &key offset) binding
                `(,var (struct-member ,%pointer ,type-name ,member-name
                                      ,@(when offset
                                          `(:offset ,offset))))))
            bindings))
     ,@body))