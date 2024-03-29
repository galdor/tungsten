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
     (error 'unknown-foreign-type :name type))))

(defun %foreign-type-size (type)
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       (%translate-to-foreign-type type) nil))
     8))

(defun %foreign-type-alignment (type)
  (/ (sb-alien-internals:alien-type-alignment
      (sb-alien-internals:parse-alien-type
       (%translate-to-foreign-type type) nil))
     8))

(defun %foreign-type-read-function (type)
  (flet ((integer-function (type signedp)
           (let ((size (%foreign-type-size type)))
             (ecase size
               (1
                (if signedp
                    'sb-sys:signed-sap-ref-8
                    'sb-sys:sap-ref-8))
               (2
                (if signedp
                    'sb-sys:signed-sap-ref-16
                    'sb-sys:sap-ref-16))
               (4
                (if signedp
                    'sb-sys:signed-sap-ref-32
                    'sb-sys:sap-ref-32))
               (8
                (if signedp
                    'sb-sys:signed-sap-ref-64
                    'sb-sys:sap-ref-64))))))
    (case type
      ((:void)
       (error "cannot reference foreign void values"))
      ((:char :short :int :long :long-long :int8 :int16 :int32 :int64)
       (integer-function type t))
      ((:unsigned-char :unsigned-short :unsigned-int :unsigned-long
        :unsigned-long-long :uint8 :uint16 :uint32 :uint64)
       (integer-function type nil))
      ((:float)
       'sb-sys:sap-ref-single)
      ((:double)
       'sb-sys:sap-ref-double)
      ((:pointer)
       'sb-sys:sap-ref-sap)
      (t
       (error 'unknown-foreign-type :name type)))))

(defun %write-foreign-type (%pointer type offset value)
  (funcall (fdefinition `(setf ,(%foreign-type-read-function type)))
           value %pointer offset))

(define-compiler-macro %write-foreign-type (&whole form
                                            %pointer type offset value)
  (if (constantp type)
      `(setf (,(%foreign-type-read-function type) ,%pointer ,offset) ,value)
      form))

;;;
;;; Memory
;;;

(deftype pointer ()
  'sb-sys:system-area-pointer)

(declaim (inline %pointer+))
(defun %pointer+ (%pointer offset)
  (sb-sys:sap+ %pointer offset))

(declaim (inline %null-pointer))
(defun %null-pointer ()
  (sb-sys:int-sap 0))

(declaim (inline %pointer))
(defun %pointer (value)
  (declare (type (or pointer (integer 0))))
  (etypecase value
    (pointer value)
    (integer (sb-sys:int-sap value))))

(declaim (inline %null-pointer-p))
(defun %null-pointer-p (%pointer)
  (zerop (sb-sys:sap-int %pointer)))

(defun %allocate-foreign-memory (size)
  (sb-alien:alien-sap
   (sb-alien:make-alien (sb-alien:unsigned 8) size)))

(defun %free-foreign-memory (%pointer)
  (sb-alien:free-alien
   (sb-alien:sap-alien %pointer (sb-alien:* (sb-alien:unsigned 8)))))

(defmacro %with-foreign-value ((%pointer type &key (count 1)) &body body)
  (let ((alien (gensym "ALIEN-")))
    `(sb-alien:with-alien
         ((,alien (sb-alien:array (sb-alien:unsigned 8)
                                  ,(* (%foreign-type-size type) count))))
       (let ((,%pointer (sb-alien:alien-sap ,alien)))
         ,@body))))

(defmacro %with-pinned-vector-data ((%pointer vector) &body body)
  (let ((vector-var (gensym "VECTOR-")))
    `(let ((,vector-var ,vector))
       (declare (type (sb-kernel:simple-unboxed-array (*)) ,vector-var))
       (sb-sys:with-pinned-objects (,vector-var)
         (let ((,%pointer (sb-sys:vector-sap ,vector-var)))
           ,@body)))))

;;;
;;; Callbacks
;;;

(defmacro %defcallback ((name ((&rest arg-types) return-type) &rest arg-names)
                        &body body)
  `(sb-alien:define-alien-callable ,name
       ,(%translate-to-foreign-type return-type)
       ,(mapcar (lambda (type name)
                  (list name (%translate-to-foreign-type type)))
                arg-types arg-names)
     (let ,(mapcar (lambda (type name)
                     `(,name ,(if (eq type :pointer)
                                  `(sb-alien:alien-sap ,name)
                                  name)))
            arg-types arg-names)
       ,@body)))

(declaim (inline %callback-pointer))
(defun %callback-pointer (name)
  (let ((alien (sb-alien:alien-callable-function name)))
    (when alien
      (sb-alien:alien-sap alien))))

;;;
;;; Foreign calls
;;;

(defmacro %foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  (let ((value (gensym "VALUE-")))
    `(let ((,value
             (sb-alien:alien-funcall
              (sb-alien:extern-alien
               ,name
               (function ,(%translate-to-foreign-type return-type)
                         ,@(mapcar #'%translate-to-foreign-type arg-types)))
              ,@args)))
       (if (eq ,return-type :pointer)
           (sb-alien:alien-sap ,value)
           ,value))))

;;;
;;; Errno
;;;

(defun %errno ()
  (sb-alien:get-errno))
