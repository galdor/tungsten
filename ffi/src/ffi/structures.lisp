(in-package :ffi)

(defclass struct (foreign-type)
  ((members
    :type list
    :initarg :members
    :reader struct-members))
  (:default-initargs
   :base-type :pointer))

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

(defmethod initialize-instance :after ((struct struct) &key)
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
      (setf (slot-value struct 'size) size
            (slot-value struct 'alignment) max-alignment))))

(defun find-struct-member (name struct)
  (or (find name (struct-members struct) :key #'struct-member-name)
      (error 'unknown-struct-member :struct struct :name name)))

(defmacro define-struct ((name) (&rest members-data))
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
      (make-instance 'struct :name ',name
                             :members (list ,@members)))))

(defun struct-member (ptr type-name member-name &optional (offset 0))
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (foreign-value (%pointer+ ptr (struct-member-offset member))
                   (struct-member-type member)
                   offset)))

(define-compiler-macro struct-member (&whole form
                                      ptr type-name member-name
                                      &optional (offset 0))
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-struct-member member-name type))
            (member-type (struct-member-type member)))
       `(foreign-value (%pointer+ ,ptr ,(struct-member-offset member))
                       ,(if (keywordp member-type) member-type `',member-type)
                       ,offset)))
    (t
     form)))

(defun write-struct-member (ptr type-name member-name offset value)
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (setf (foreign-value (%pointer+ ptr (struct-member-offset member))
                         (struct-member-type member)
                         offset)
          value)))

(define-compiler-macro write-struct-member (&whole form
                                            ptr type-name member-name offset
                                            value)
  (cond
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp member-name))
     (let* ((type (foreign-type (cadr type-name)))
            (member (find-struct-member member-name type))
            (member-type (struct-member-type member)))
       `(setf (foreign-value (%pointer+ ,ptr ,(struct-member-offset member))
                             ,(if (keywordp member-type)
                                  member-type
                                  `',member-type)
                             ,offset)
              ,value)))
    (t
     form)))

(defsetf struct-member (ptr type-name member-name &optional (offset 0))
    (value)
  `(write-struct-member ,ptr ,type-name ,member-name ,offset ,value))

(defun struct-member-pointer (ptr type-name member-name &optional (offset 0))
  (let* ((type (foreign-type type-name))
         (member (find-struct-member member-name type)))
    (%pointer+ ptr (+ (struct-member-offset member)
                      (* offset (foreign-type-size
                                 (struct-member-type member)))))))

(define-compiler-macro struct-member-pointer (&whole form
                                              ptr type-name member-name
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
         ,ptr
         ,(if (constantp offset)
              (+ (struct-member-offset member)
                  (* offset (foreign-type-size member-type)))
              `(+ ,(struct-member-offset member)
                  (* ,offset ,(foreign-type-size member-type)))))))
    (t
     form)))
