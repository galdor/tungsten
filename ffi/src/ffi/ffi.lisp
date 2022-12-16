(in-package :ffi)

;;;
;;; Memory
;;;

(defun null-pointer ()
  (%null-pointer))

(defun null-pointer-p (%pointer)
  (%null-pointer-p %pointer))

(defun allocate-foreign-memory (size)
  (%allocate-foreign-memory size))

(defun free-foreign-memory (size)
  (%free-foreign-memory size))

(defmacro with-foreign-value ((%pointer type-name &key (count 1)) &body body)
  (cond
    ((and (constantp type-name) (base-type-p type-name)
          (constantp count) (integerp count))
     `(%with-foreign-value (,%pointer ,(foreign-base-type type-name)
                                      :count ,count)
        ,@body))
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (constantp count) (integerp count))
     `(%with-foreign-value (,%pointer ,(foreign-base-type (cadr type-name))
                                      :count ,count)
        ,@body))
    (t
     `(let ((,%pointer (%allocate-foreign-memory
                        (* (foreign-type-size ,type-name) ,count))))
        (unwind-protect
             (progn ,@body)
          (%free-foreign-memory ,%pointer))))))

(defmacro with-foreign-values ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-value ,(car bindings)
         (with-foreign-values ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

(defun write-foreign-value (%pointer type-name offset value)
  (let* ((type (gethash type-name *foreign-types*))
         (base-type (if type (foreign-type-base-type type) type-name))
         (type-size (%foreign-type-size base-type))
         (encoded-value
           (if (and type (foreign-type-encoder type))
               (funcall (foreign-type-encoder type) type value)
               value)))
    (%write-foreign-type %pointer base-type (* type-size offset) encoded-value)
    value))

(define-compiler-macro write-foreign-value (&whole form
                                            %pointer type-name offset value)
  (cond
    ((and (constantp type-name)
          (base-type-p type-name))
     `(%write-foreign-type
       ,%pointer
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
         ,%pointer
         ,base-type
         ,(if (constantp offset)
              (* type-size offset)
              `(* ,type-size ,offset))
         ,(if (foreign-type-encoder type)
              (if (constantp value)
                  (funcall (foreign-type-encoder type) type value)
                  `(,(foreign-type-encoder type)
                    (foreign-type ',(cadr type-name))
                    ,value))
              value))))
    (t
     form)))

(defun foreign-value (%pointer type-name &optional (offset 0))
  (let* ((type (gethash type-name *foreign-types*))
         (base-type (if type (foreign-type-base-type type) type-name))
         (read-function (%foreign-type-read-function base-type))
         (type-size (%foreign-type-size base-type))
         (value (funcall read-function %pointer (* type-size offset))))
    (if (and type (foreign-type-decoder type))
        (funcall (foreign-type-decoder type) type value)
        value)))

(define-compiler-macro foreign-value (&whole form
                                      %pointer type-name &optional (offset 0))
  (cond
    ((and (constantp type-name)
          (base-type-p type-name))
     `(,(%foreign-type-read-function type-name)
       ,%pointer
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
       (if (foreign-type-decoder type)
           `(,(foreign-type-decoder type)
             (foreign-type ',(cadr type-name))
             (,(%foreign-type-read-function base-type)
              ,%pointer ,offset-form))
           `(,(%foreign-type-read-function base-type)
             ,%pointer ,offset-form))))
    (t
     form)))

(defsetf foreign-value (%pointer type-name &optional (offset 0))
    (value)
  `(write-foreign-value ,%pointer ,type-name ,offset ,value))

(defun read-foreign-memory (%pointer size)
  "Read SIZE octets of foreign memory starting at %POINTER and return them as an
octet array."
  (declare (type (integer 0) size))
  (let ((octets (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size octets)
      (setf (aref octets i) (foreign-value %pointer :uint8 i)))))

(defun clear-foreign-memory (%pointer size)
  "Clear SIZE octets of foreign memory starting at %POINTER by setting them to
zero."
  (declare (type (integer 0) size))
  (dotimes (i size size)
    (setf (foreign-value %pointer :uint8 i) 0)))

(defmacro with-pinned-vector-data ((%pointer vector) &body body)
  `(%with-pinned-vector-data (,%pointer ,vector)
     ,@body))

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
         (%pointer (allocate-foreign-memory (1+ (length octets))))
         (success nil))
    (unwind-protect
         (progn
           (dotimes (i nb-octets)
             (setf (foreign-value %pointer :uint8 i) (aref octets i)))
           (setf (foreign-value %pointer :uint8 nb-octets) 0)
           (setf success t)
           (values %pointer nb-octets))
      (unless success
        (free-foreign-memory %pointer)))))

(defmacro with-foreign-string ((var-or-vars string
                                &key (encoding *default-string-encoding*)
                                     start end)
                               &body body)
  (destructuring-bind (%pointer &optional (length-var (gensym "LENGTH-")))
      (if (listp var-or-vars) var-or-vars (list var-or-vars))
    `(multiple-value-bind (,%pointer ,length-var)
         (allocate-foreign-string ,string :encoding ,encoding
                                          :start ,start :end ,end)
       (declare (ignorable ,length-var))
       (unwind-protect
            (progn
              ,@body)
         (free-foreign-memory ,%pointer)))))

(defmacro with-foreign-strings ((&rest bindings) &body body)
  (if bindings
      `(with-foreign-string ,(car bindings)
         (with-foreign-strings ,(cdr bindings)
           ,@body))
      `(progn
         ,@body)))

(defun foreign-string-length (%pointer &key (offset 0))
  (do ((i offset (1+ i)))
      ((zerop (foreign-value %pointer :uint8 i))
       (- i offset))))

(defun decode-foreign-string (%pointer
                              &key (encoding *default-string-encoding*)
                                   (offset 0) length)
  (unless length
    (setf length (foreign-string-length %pointer :offset offset)))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref octets i) (foreign-value %pointer :uint8 (+ offset i))))
    (text:decode-string octets :encoding encoding)))

;;;
;;; Foreign calls
;;;

(defmacro foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  (flet ((expand-arg (arg type-name)
           (if (base-type-p type-name)
               arg
               (let* ((type (foreign-type type-name))
                      (encoder (foreign-type-encoder type)))
                 (if encoder
                     `(,encoder (foreign-type ',type-name) ,arg)
                     arg)))))
    (let ((nb-args (length args))
          (nb-arg-types (length arg-types)))
      (unless (= nb-args nb-arg-types)
        (error "cannot match ~D arguments to ~D argument types"
               nb-args nb-arg-types)))
    (let ((value (gensym "VALUE-")))
      `(let ((,value
               (%foreign-funcall ,name
                                 (,(mapcar #'foreign-base-type arg-types)
                                   ,(foreign-base-type return-type))
                                 ,@(mapcar #'expand-arg args arg-types))))
         ,(if (base-type-p return-type)
              value
              (let* ((type (foreign-type return-type))
                     (decoder (foreign-type-decoder type)))
                (if decoder
                    `(,decoder (foreign-type ',return-type) ,value)
                    value)))))))
