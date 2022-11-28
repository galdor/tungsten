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

(define-foreign-type-alias
    :size
  #+32-bit-target :uint32
  #+64-bit-target :uint64
  #-(or 32-bit-target 64-bit-target)
  (error "missing 32-BIT-TARGET or 64-BIT-TARGET in *FEATURES*"))

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

(defun %write-foreign-type (ptr type offset value)
  (funcall (%foreign-type-write-function type) ptr offset value))

(define-compiler-macro %write-foreign-type (&whole form ptr type offset value)
  (if (constantp type)
      `(,(%foreign-type-write-function type) ,ptr ,offset ,value)
      form))

;;;
;;; Memory
;;;

(defun %null-pointer ()
  (ccl:%null-ptr))

(defun %null-pointer-p (ptr)
  (ccl:%null-ptr-p ptr))

(defun %allocate-foreign-memory (size)
  (ccl::malloc size))

(defun %free-foreign-memory (ptr)
  (ccl::free ptr))

(defmacro %with-foreign-value ((ptr-var type &key (count 1)) &body body)
  `(ccl:%stack-block ((,ptr-var ,(* (%foreign-type-size type) count)))
     ,@body))

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
