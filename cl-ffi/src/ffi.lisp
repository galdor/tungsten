(in-package :ffi)

;;;
;;; Types
;;;

(defun foreign-type-size (type-name)
  (let ((type (gethash type-name *foreign-types*)))
    (if type
        (foreign-type-size (foreign-type-base-type type))
        (%foreign-type-size type-name))))

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

(defmacro with-foreign-value ((ptr-var type &key (count 1)) &body body)
  (if (and (constantp type)
           (base-type-p type)
           (constantp count))
      `(%with-foreign-value (,ptr-var ,type :count ,count)
         ,@body)
      `(let ((,ptr-var (%allocate-foreign-memory
                        (* (foreign-type-size ,type) ,count))))
         (unwind-protect
              (progn ,@body)
           (%free-foreign-memory ,ptr-var)))))

(defmacro with-foreign-values ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-value ,(car bindings)
         (with-foreign-values ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

(declaim (inline read-foreign-value))
(defun read-foreign-value (ptr type-name offset)
  (let* ((type (gethash type-name *foreign-types*))
         (base-type (if type (foreign-type-base-type type) type-name))
         (read-function (%foreign-type-read-function base-type))
         (type-size (%foreign-type-size base-type))
         (value (funcall read-function ptr (* type-size offset))))
    (if (and type (slot-boundp type 'decoder))
        (funcall (foreign-type-decoder type) type value)
        value)))

(define-compiler-macro read-foreign-value (&whole form ptr type-name offset)
  (if (constantp type-name)
      (let* ((type (gethash type-name *foreign-types*))
             (base-type (if type (foreign-type-base-type type) type-name))
             (offset-form
               (if (constantp offset)
                   (* (%foreign-type-size base-type) offset)
                   `(* ,(%foreign-type-size base-type) ,offset))))
        (if (and type (slot-boundp type 'decoder))
            `(,(foreign-type-decoder type)
              type
              `(,(%foreign-type-read-function base-type) ,ptr ,offset-form))
            `(,(%foreign-type-read-function base-type) ,ptr ,offset-form)))
      form))

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
  (if (constantp type-name)
      (let* ((type (gethash type-name *foreign-types*))
             (base-type (if type (foreign-type-base-type type) type-name))
             (offset-form
               (if (constantp offset)
                   (* (%foreign-type-size base-type) offset)
                   `(* ,(%foreign-type-size base-type) ,offset))))
        `(%write-foreign-type
          ,ptr
          ,base-type
          ,offset-form
          ,(if (and type (slot-boundp type 'encoder))
               `(,(foreign-type-encoder type) ,type ,value)
               value)))
      form))

(defun foreign-value-ref (ptr type-name &optional (offset 0))
  (read-foreign-value ptr type-name offset))

(define-compiler-macro foreign-value-ref (ptr type-name &optional (offset 0))
  `(read-foreign-value ,ptr ,type-name ,offset))

(defsetf foreign-value-ref (ptr type-name &optional (offset 0))
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
            (setf (foreign-value-ref ptr :uint8 i) (aref octets i)))
          (setf (foreign-value-ref ptr :uint8 nb-octets) 0)
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
      ((zerop (foreign-value-ref ptr :uint8 i))
       (- i offset))))

(defun decode-foreign-string (ptr &key (encoding *default-string-encoding*)
                                       (offset 0) length)
  (unless length
    (setf length (foreign-string-length ptr :offset offset)))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref octets i) (foreign-value-ref ptr :uint8 (+ offset i))))
    (text:decode-string octets :encoding encoding)))

;;;
;;; Foreign calls
;;;

(defmacro foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  `(%foreign-funcall ,name
                     (,(mapcar #'foreign-base-type arg-types)
                      ,(foreign-base-type return-type))
                     ,@args))
