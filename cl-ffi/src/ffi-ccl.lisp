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
     :unsigned-halfworld)
    ((:int32)
     :signed-fullworld)
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
        (error "Unsupported foreign type ~A.~%" type))))))

(defun %foreign-type-size (type)
  `(ccl::foreign-size ,(%translate-to-foreign-type type) :bytes))

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
