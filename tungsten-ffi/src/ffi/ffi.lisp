(in-package :ffi)

;;;
;;; Memory
;;;

(declaim (inline pointer+))
(defun pointer+ (%pointer offset)
  (%pointer+ %pointer offset))

(declaim (inline null-pointer))
(defun null-pointer ()
  (%null-pointer))

(declaim (inline null-pointer-p))
(defun null-pointer-p (%pointer)
  (%null-pointer-p %pointer))

(defun allocate-foreign-memory (size)
  (%allocate-foreign-memory size))

(defun free-foreign-memory (size)
  (%free-foreign-memory size))

(defmacro with-foreign-value ((%pointer type-name &key (count 1)) &body body)
  ;; Note that we do not use %WITH-FOREIGN-VALUE if the type size is not equal
  ;; to the base type size (e.g. for structures). It is a bit of a hack; we
  ;; would need to mark types has having a fake base type (:POINTER for
  ;; structures). But it is not really fake: structures are manipulated as
  ;; pointers after all.
  (cond
    ((and (constantp type-name) (base-type-p type-name)
          (constantp count) (integerp count))
     `(%with-foreign-value (,%pointer ,(foreign-base-type type-name)
                                      :count ,count)
        ,@body))
    ((and (listp type-name)
          (eq (car type-name) 'cl:quote)
          (symbolp (cadr type-name))
          (= (foreign-type-size (cadr type-name))
             (foreign-type-size (foreign-base-type (cadr type-name))))
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

(defmacro with-pinned-vector-data ((%pointer vector &optional (offset 0))
                                   &body body)
  (let ((offset-var (gensym "OFFSET-")))
    `(let ((,offset-var ,offset))
       (%with-pinned-vector-data (,%pointer ,vector)
         (setf ,%pointer (pointer+ ,%pointer ,offset-var))
         ,@body))))

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
         (%pointer (allocate-foreign-memory (1+ (length octets)))))
    (core:abort-protect
        (progn
          (dotimes (i nb-octets)
            (setf (foreign-value %pointer :uint8 i) (aref octets i)))
          (setf (foreign-value %pointer :uint8 nb-octets) 0)
          (values %pointer nb-octets))
      (free-foreign-memory %pointer))))

(defmacro with-foreign-string ((var-or-vars string
                                &key (encoding *default-string-encoding*)
                                     start end)
                               &body body)
  (let ((string-var (gensym "STRING-")))
    (destructuring-bind (%pointer &optional (length-var (gensym "LENGTH-")))
        (if (listp var-or-vars) var-or-vars (list var-or-vars))
      `(let ((,string-var ,string))
         (multiple-value-bind (,%pointer ,length-var)
             (if ,string-var
                 (allocate-foreign-string ,string :encoding ,encoding
                                                  :start ,start :end ,end)
                 (values (ffi:null-pointer) 0))
           (declare (ignorable ,length-var))
           (unwind-protect
                (progn
                  ,@body)
             (free-foreign-memory ,%pointer)))))))

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
;;; Callbacks
;;;

(define-condition unknown-callback (error)
  ((name
    :type symbol
    :initarg :name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown callback ~S." name)))))

(defmacro defcallback ((name ((&rest arg-types) return-type) &rest arg-names)
                       &body body)
  (let ((nb-arg-names (length arg-names))
        (nb-arg-types (length arg-types)))
    (unless (= nb-arg-names nb-arg-types)
      (error "cannot match ~D argument names to ~D argument types"
             nb-arg-names nb-arg-types)))
  (let ((value (gensym "VALUE-")))
    `(%defcallback (,name ((,@(mapcar 'foreign-base-type arg-types))
                           ,(foreign-base-type return-type))
                          ,@arg-names)
       (let ,(mapcar
              (lambda (name type-name)
                (if (base-type-p type-name)
                    `(,name ,name)
                    (let* ((type (foreign-type type-name))
                           (decoder (foreign-type-decoder type)))
                      (if decoder
                          `(,name
                            (,decoder (foreign-type ',type-name) ,name))
                          `(,name ,name)))))
              arg-names arg-types)
         (let ((,value (progn ,@body)))
           ,(if (base-type-p return-type)
                value
                (let* ((type (foreign-type return-type))
                       (decoder (foreign-type-decoder type)))
                  (if decoder
                      `(,decoder (foreign-type ',return-type) ,value)
                      value))))))))

(defun callback-pointer (name)
  (let ((pointer (%callback-pointer name)))
    (unless pointer
      (error 'unknown-callback :name name))
    pointer))

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
                                 (,(mapcar 'foreign-base-type arg-types)
                                  ,(foreign-base-type return-type))
                                 ,@(mapcar #'expand-arg args arg-types))))
         ,(if (base-type-p return-type)
              value
              (let* ((type (foreign-type return-type))
                     (decoder (foreign-type-decoder type)))
                (if decoder
                    `(,decoder (foreign-type ',return-type) ,value)
                    value)))))))
