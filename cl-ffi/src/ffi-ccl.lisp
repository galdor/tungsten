(in-package :ffi)

;;;
;;; Shared libraries
;;;

(defun %load-foreign-library (path)
  (ccl:open-shared-library path))

(defun %unload-foreign-library (handle)
  (ccl:close-shared-library handle))

;;;
;;; Types
;;;

(defun %translate-to-foreign-type (type)
  (case type
    ((:void)
     :void)
    ((:char)
     :signed-byte)
    ((:unsigned-char)
     :unsigned-byte)
    ((:short)
     :signed-short)
    ((:unsigned-short)
     :unsigned-short)
    ((:int)
     :signed-int)
    ((:unsigned-int)
     :unsigned-int)
    ((:long)
     :signed-long)
    ((:unsigned-long)
     :unsigned-long)
    ((:long-long)
     :signed-doubleword)
    ((:unsigned-long-long)
     :unsigned-doubleword)
    ((:int8)
     :signed-byte)
    ((:uint8)
     :unsigned-byte)
    ((:int16)
     :signed-halfword)
    ((:uint16)
     :unsigned-halfword)
    ((:int32)
     :signed-fullword)
    ((:uint32)
     :unsigned-fullword)
    ((:int64)
     :signed-doubleword)
    ((:uint64)
     :unsigned-doubleword)
    ((:float)
     :single-float)
    ((:double)
     :double-float)
    ((:pointer)
     '(:* t))
    (t
     (cond
       ((and (listp type)
             (= (length type) 2)
             (eq (car type) :pointer))
        `(:* ,(%translate-to-foreign-type (cadr type))))
       ((and (listp type)
             (= (length type) 3)
             (eq (first type) :array))
        `(:array ,(%translate-to-foreign-type (second type)) ,(third type)))
       (t
        (error "unsupported foreign type ~A" type))))))

(defun %foreign-type-size (type)
  (/ (ccl::foreign-type-bits
      (ccl::parse-foreign-type
       (%translate-to-foreign-type type)))
     8))

(defun %foreign-type-ref-function (type)
  (case type
    ((:void)
     (error "cannot reference foreign void values"))
    ((:char :int8)
     'ccl:%get-signed-byte)
    ((:unsigned-char :uint8)
     'ccl:%get-unsigned-byte)
    ((:short :int16)
     'ccl:%get-signed-word)
    ((:unsigned-short :uint16)
     'ccl:%get-unsigned-word)
    ((:int :int32)
     'ccl:%get-signed-long)
    ((:unsigned-int :uint32)
     'ccl:%get-unsigned-long)
    ((:long)
     #+32-bit-target '%get-signed-long
     #+64-bit-target '%get-signed-long-long)
    ((:unsigned-long)
     #+32-bit-target '%get-unsigned-long
     #+64-bit-target '%get-unsigned-long-long)
    ((:long-long :int64)
     'ccl::%get-signed-long-long)
    ((:unsigned-long-long :uint64)
     'ccl::%get-unsigned-long-long)
    ((:float)
     'ccl:%get-single-float)
    ((:double)
     'ccl:%get-double-float)
    ((:pointer)
     'ccl:%get-ptr)
    (t
     (cond
       ((and (listp type)
             (= (length type) 2)
             (eq (car type) :pointer))
        'ccl:%get-ptr)
       (t
        (error "cannot reference foreign values of type ~A" type))))))

;;;
;;; Memory
;;;

(defun %allocate-foreign-memory (size)
  (ccl::malloc size))

(defun %free-foreign-memory (ptr)
  (ccl::free ptr))

(defmacro %with-foreign-value ((ptr-var type) &body body)
  `(ccl:%stack-block ((,ptr-var ,(%foreign-type-size type)))
     ,@body))

;;;
;;; Foreign calls
;;;

(defmacro %foreign-funcall (name (&rest arg-types) return-type &rest args)
  `(ccl:external-call
    ,name
    ,@(apply #'append (mapcar #'list arg-types args))
    ,(%translate-to-foreign-type return-type)))
