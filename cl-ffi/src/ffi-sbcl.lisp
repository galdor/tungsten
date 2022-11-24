(in-package :ffi)

;;;
;;; Shared libraries
;;;

(defun %load-foreign-library (path)
  (sb-alien:load-shared-object path))

(defun %unload-foreign-library (handle)
  (sb-alien:unload-shared-object handle))

;;;
;;; Types
;;;
(defun %translate-to-foreign-type (type)
  (case type
    ((:void)
     'sb-alien:void)
    ((:char)
     'sb-alien:char)
    ((:unsigned-char)
     'sb-alien:unsigned-char)
    ((:short)
     'sb-alien:short)
    ((:unsigned-short)
     'sb-alien:unsigned-short)
    ((:int)
     'sb-alien:int)
    ((:unsigned-int)
     'sb-alien:unsigned-int)
    ((:long)
     'sb-alien:long)
    ((:unsigned-long)
     'sb-alien:unsigned-long)
    ((:long-long)
     'sb-alien:long-long)
    ((:unsigned-long-long)
     'sb-alien:unsigned-long-long)
    ((:int8)
     '(sb-alien:integer 8))
    ((:uint8)
     '(sb-alien:unsigned 8))
    ((:int16)
     '(sb-alien:integer 16))
    ((:uint16)
     '(sb-alien:unsigned 16))
    ((:int32)
     '(sb-alien:integer 32))
    ((:uint32)
     '(sb-alien:unsigned 32))
    ((:int64)
     '(sb-alien:integer 64))
    ((:uint64)
     '(sb-alien:unsigned 64))
    ((:float)
     'sb-alien:single-float)
    ((:double)
     'sb-alien:double-float)
    ((:pointer)
     '(sb-alien:* t))
    (t
     (cond
       ((and (listp type)
             (= (length type) 2)
             (eq (car type) :pointer))
        `(sb-alien:* ,(%translate-to-foreign-type (cadr type))))
       ((and (listp type)
             (= (length type) 3)
             (eq (first type) :array))
        `(sb-alien:array ,(%translate-to-foreign-type (second type))
                         ,(third type)))
       (t
        (error "Unsupported foreign type ~A.~%" type))))))

(defun %foreign-type-size (type)
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (%translate-to-foreign-type type) nil))
     8))

;;;
;;; Memory
;;;

(defun %allocate-foreign-memory (size)
  (sb-alien:alien-sap
   (sb-alien:make-alien (sb-alien:unsigned 8) size)))

(defun %free-foreign-memory (ptr)
  (sb-alien:free-alien
   (sb-alien:sap-alien ptr (sb-alien:* (sb-alien:unsigned 8)))))

(defmacro %with-foreign-value ((ptr-var type) &body body)
  (let ((alien-var (gensym "ALIEN-")))
    `(sb-alien:with-alien
         ((,alien-var
           (sb-alien:array (sb-alien:unsigned 8) ,(%foreign-type-size type))))
       (let ((,ptr-var (sb-alien:alien-sap ,alien-var)))
         ,@body))))

;;;
;;; Foreign calls
;;;

(defmacro %foreign-funcall (name (&rest arg-types) return-type &rest args)
  `(sb-alien:alien-funcall
    (sb-alien:extern-alien
     ,name
     (function ,(%translate-to-foreign-type return-type)
               ,@(mapcar #'%translate-to-foreign-type arg-types)))
    ,@args))
