(in-package :ffi)

;;;
;;; Types
;;;

(defun foreign-type-size (type)
  (%foreign-type-size type))

;;;
;;; Memory
;;;

(defun allocate-foreign-memory (size)
  (%allocate-foreign-memory size))

(defun free-foreign-memory (size)
  (%free-foreign-memory size))

(defmacro with-foreign-value ((ptr-var type &key (count 1)) &body body)
  (if (and (constantp type) (constantp count))
      `(%with-foreign-value (,ptr-var ,type :count ,count)
         ,@body)
      `(let ((,ptr-var (%allocate-foreign-memory
                        (* (%foreign-type-size ,type) ,count))))
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

(defmacro foreign-value-ref (ptr type &optional (offset 0))
  (if (constantp offset)
      `(,(%foreign-type-ref-function type)
        ,ptr
        ,(* (%foreign-type-size type) offset))
      `(,(%foreign-type-ref-function type)
        ,ptr
        (* (%foreign-type-size ,type) ,offset))))

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
  `(%foreign-funcall ,name ((,@arg-types) ,return-type) ,@args))
