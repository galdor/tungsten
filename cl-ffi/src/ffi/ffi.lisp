(in-package :ffi)

;;; Note that in several macros and compiler macros, We cannot expand when the
;;; type passed as argument is a symbol. For example, (FOREIGN-TYPE-SIZE
;;; 'FOO:BAR) is read as (FOREIGN-TYPE-SIZE (QUOTE FOO::BAR)) during macro
;;; expansion. So we do not expand the expression when the type argument is a
;;; list.
;;;
;;; There must be a smart way to handle it though.

;;;
;;; Memory
;;;

(defun null-pointer ()
  (%null-pointer))

(defun null-pointer-p (ptr)
  (%null-pointer-p ptr))

(defun allocate-foreign-memory (size)
  (%allocate-foreign-memory size))

(defun free-foreign-memory (size)
  (%free-foreign-memory size))

(defmacro with-foreign-value ((ptr-var type-name &key (count 1)) &body body)
  (cond
    ((and (constantp type-name)
          (base-type-p type-name))
     `(%with-foreign-value (,ptr-var ,(foreign-base-type type-name)
                                     :count ,count)
        ,@body))
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name)))
     `(%with-foreign-value (,ptr-var ,(foreign-base-type (cadr type-name))
                                     :count ,count)
        ,@body))
    (t
     `(let ((,ptr-var (%allocate-foreign-memory
                        (* (foreign-type-size ,type-name) ,count))))
         (unwind-protect
              (progn ,@body)
           (%free-foreign-memory ,ptr-var))))))

(defmacro with-foreign-values ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-value ,(car bindings)
         (with-foreign-values ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

(defun write-foreign-value (ptr type-name offset value)
  (let* ((type (gethash type-name *foreign-types*))
         (base-type (if type (foreign-type-base-type type) type-name))
         (type-size (%foreign-type-size base-type))
         (encoded-value
           (if (and type (slot-boundp type 'encoder))
               (funcall (foreign-type-encoder type) type value)
               value)))
    (%write-foreign-type ptr base-type (* type-size offset) encoded-value)
    value))

(define-compiler-macro write-foreign-value (&whole form
                                            ptr type-name offset value)
  (cond
    ((and (constantp type-name)
          (base-type-p type-name))
     `(%write-foreign-type
       ,ptr
       ,type-name
       ,(if (constantp offset)
            (* (%foreign-type-size type-name) offset)
            `(* ,(%foreign-type-size type-name) ,offset))
       ,value))
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name)))
     (let* ((type (foreign-type (cadr type-name)))
            (type-size (slot-value type 'size))
            (base-type (foreign-type-base-type type)))
       `(%write-foreign-type
         ,ptr
         ,base-type
         ,(if (constantp offset)
              (* type-size offset)
              `(* ,type-size ,offset))
         ,(if (slot-boundp type 'encoder)
              (if (constantp value)
                  (funcall (foreign-type-encoder type) type value)
                  `(,(foreign-type-encoder type)
                    (foreign-type ',(cadr type-name))
                    ,value))
              value))))
    (t
     form)))

(defun foreign-value (ptr type-name &optional (offset 0))
  (let* ((type (gethash type-name *foreign-types*))
         (base-type (if type (foreign-type-base-type type) type-name))
         (read-function (%foreign-type-read-function base-type))
         (type-size (%foreign-type-size base-type))
         (value (funcall read-function ptr (* type-size offset))))
    (if (and type (slot-boundp type 'decoder))
        (funcall (foreign-type-decoder type) type value)
        value)))

(define-compiler-macro foreign-value (&whole form
                                      ptr type-name &optional (offset 0))
  (cond
    ((and (constantp type-name)
          (base-type-p type-name))
     `(,(%foreign-type-read-function type-name)
       ,ptr
       ,(if (constantp offset)
            (* (%foreign-type-size type-name) offset)
            `(* ,(%foreign-type-size type-name) ,offset))))
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name)))
     (let* ((type (foreign-type (cadr type-name)))
            (type-size (slot-value type 'size))
            (base-type (foreign-type-base-type type))
            (offset-form (if (constantp offset)
                             (* type-size offset)
                             `(* ,type-size ,offset))))
       (if (slot-boundp type 'decoder)
           `(,(foreign-type-decoder type)
             (foreign-type ',(cadr type-name))
             (,(%foreign-type-read-function base-type) ,ptr ,offset-form))
           `(,(%foreign-type-read-function base-type) ,ptr ,offset-form))))
    (t
     form)))

(defsetf foreign-value (ptr type-name &optional (offset 0))
    (value)
  `(write-foreign-value ,ptr ,type-name ,offset ,value))

;;;
;;; Strings
;;;

(defparameter *default-string-encoding* text:*default-encoding*)

(defun allocate-foreign-string (string
                                &key (encoding *default-string-encoding*)
                                     start end)
  (let* ((octets (text:encode-string string :encoding encoding
                                            :start start :end end))
         (nb-octets (length octets))
         (ptr (allocate-foreign-memory (1+ (length octets)))))
    (with-cleanup
        (progn
          (dotimes (i nb-octets)
            (setf (foreign-value ptr :uint8 i) (aref octets i)))
          (setf (foreign-value ptr :uint8 nb-octets) 0)
          (values ptr nb-octets))
      (free-foreign-memory ptr))))

(defmacro with-foreign-string ((var-or-vars string
                                &key (encoding *default-string-encoding*)
                                     start end)
                               &body body)
  (destructuring-bind (ptr-var &optional (length-var (gensym "LENGTH-")))
      (if (listp var-or-vars) var-or-vars (list var-or-vars))
    `(multiple-value-bind (,ptr-var ,length-var)
         (allocate-foreign-string ,string :encoding ,encoding
                                          :start ,start :end ,end)
       (declare (ignorable ,length-var))
       (unwind-protect
            (progn
              ,@body)
         (free-foreign-memory ,ptr-var)))))

(defmacro with-foreign-strings ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-string ,(car bindings)
         (with-foreign-strings ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

(defun foreign-string-length (ptr &key (offset 0))
  (do ((i offset (1+ i)))
      ((zerop (foreign-value ptr :uint8 i))
       (- i offset))))

(defun decode-foreign-string (ptr &key (encoding *default-string-encoding*)
                                       (offset 0) length)
  (unless length
    (setf length (foreign-string-length ptr :offset offset)))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref octets i) (foreign-value ptr :uint8 (+ offset i))))
    (text:decode-string octets :encoding encoding)))

;;;
;;; Foreign calls
;;;

(defmacro foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  `(%foreign-funcall ,name
                     (,(mapcar #'foreign-base-type arg-types)
                      ,(foreign-base-type return-type))
                     ,@args))
