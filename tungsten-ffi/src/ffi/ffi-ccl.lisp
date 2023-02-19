(in-package :ffi)

;;;
;;; Shared libraries
;;;

(defun %load-foreign-library (path)
  ;; CCL:OPEN-SHARED-LIBRARY does not support pathnames
  (ccl:open-shared-library (namestring path)))

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
     (error 'unknown-foreign-type :name type))))

(defun %foreign-type-size (type)
  (/ (ccl::foreign-type-bits
      (ccl::parse-foreign-type
       (%translate-to-foreign-type type)))
     8))

(defun %foreign-type-alignment (type)
  (/ (ccl::foreign-type-alignment
      (ccl::parse-foreign-type
       (%translate-to-foreign-type type)))
     8))

(defun %foreign-type-read-function (type)
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
     #+32-bit-target 'ccl:%get-signed-long
     #+64-bit-target 'ccl::%get-signed-long-long)
    ((:unsigned-long)
     #+32-bit-target 'ccl:%get-unsigned-long
     #+64-bit-target 'ccl::%get-unsigned-long-long)
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
     (error 'unknown-foreign-type :name type))))

(defun %foreign-type-write-function (type)
  (case type
    ((:void)
     (error "cannot reference foreign void values"))
    ((:char :int8)
     'ccl::%set-byte)
    ((:unsigned-char :uint8)
     'ccl::%set-unsigned-byte)
    ((:short :int16)
     'ccl::%set-word)
    ((:unsigned-short :uint16)
     'ccl::%set-unsigned-word)
    ((:int :int32)
     'ccl::%set-long)
    ((:unsigned-int :uint32)
     'ccl::%set-unsigned-long)
    ((:long)
     #+32-bit-target 'ccl::%set-long
     #+64-bit-target 'ccl::%set-signed-long-long)
    ((:unsigned-long)
     #+32-bit-target 'ccl::%set-unsigned-long
     #+64-bit-target 'ccl::%set-unsigned-long-long)
    ((:long-long :int64)
     'ccl::%set-signed-long-long)
    ((:unsigned-long-long :uint64)
     'ccl::%set-unsigned-long-long)
    ((:float)
     'ccl::%set-single-float)
    ((:double)
     'ccl::%set-double-float)
    ((:pointer)
     'ccl::%set-ptr)
    (t
     (error 'unknown-foreign-type :name type))))

(defun %write-foreign-type (%pointer type offset value)
  (funcall (%foreign-type-write-function type) %pointer offset value))

(define-compiler-macro %write-foreign-type (&whole form
                                            %pointer type offset value)
  (if (constantp type)
      `(,(%foreign-type-write-function type) ,%pointer ,offset ,value)
      form))

(defmacro %with-pinned-vector-data ((%pointer vector) &body body)
  `(ccl:with-pointer-to-ivector (,%pointer ,vector)
     ,@body))

;;;
;;; Memory
;;;

(deftype pointer ()
  'ccl:macptr)

(defun %pointer+ (%pointer offset)
  (ccl:%inc-ptr %pointer offset))

(defun %null-pointer ()
  (ccl:%null-ptr))

(declaim (inline %pointer))
(defun %pointer (value)
  (declare (type (or pointer (integer 0))))
  (etypecase value
    (pointer value)
    (integer (ccl:%int-to-ptr value))))

(defun %null-pointer-p (%pointer)
  (ccl:%null-ptr-p %pointer))

(defun %allocate-foreign-memory (size)
  (ccl::malloc size))

(defun %free-foreign-memory (%pointer)
  (ccl::free %pointer))

(defmacro %with-foreign-value ((%pointer type &key (count 1)) &body body)
  `(ccl:%stack-block ((,%pointer ,(* (%foreign-type-size type) count)))
     ,@body))

;;;
;;; Callbacks
;;;

(defvar *callback-pointers* (make-hash-table))

(defmacro %defcallback ((name ((&rest arg-types) return-type) &rest arg-names)
                        &body body)
  `(setf (gethash ',name *callback-pointers*)
         (symbol-value
          (ccl:defcallback ,name
            (,@(apply #'append
                      (mapcar (lambda (type name)
                                `(,(%translate-to-foreign-type type) ,name))
                              arg-types arg-names))
             ,(%translate-to-foreign-type return-type))
            ,@body))))

(declaim (inline %callback-pointer))
(defun %callback-pointer (name)
  (gethash name *callback-pointers*))

;;;
;;; Foreign calls
;;;

(defmacro %foreign-funcall (name ((&rest arg-types) return-type) &rest args)
  `(ccl:external-call
    ,name
    ,@(apply #'append
             (mapcar #'list
                     (mapcar #'%translate-to-foreign-type arg-types)
                     args))
    ,(%translate-to-foreign-type return-type)))

;;;
;;; Errno
;;;

(defun %errno ()
  (ccl:get-errno))
