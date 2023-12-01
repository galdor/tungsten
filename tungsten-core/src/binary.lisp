(in-package :core)

;;; It would be nice to implement floating point decoding and encoding the
;;; right way. But the right way is very tricky, so we let the FFI layer (and
;;; therefore the CPU) do all the work. Another advantage is that we let the
;;; implementation handle special values such as NaN or infinity; meaning that
;;; binary coding and FFI layer will be consistent.
;;;
;;; We cannot depend on the ffi system because it would cause a circular
;;; dependency, but code duplication is minimal, and it is not as if there was
;;; dozens of implementations to support.

(define-condition unknown-binary-type (error)
  ((type
    :initarg :type
    :reader unknown-binary-type-type))
  (:report
   (lambda (condition stream)
     (format stream "unknown binary type ~S"
             (unknown-binary-type-type condition)))))

(defmacro define-binref-integer-functions (type-size signed-p endianness)
  (let* ((nb-octets type-size)
         (nb-bits (* nb-octets 8))
         (suffix (format nil "~:[U~;~]INT~D~@[~A~]" signed-p nb-bits
                         (case endianness
                           (:big-endian "BE")
                           (:little-endian "LE"))))
         (read-function
           (intern (concatenate 'string "BINREF-READ/" suffix)))
         (write-function
           (intern (concatenate 'string "BINREF-WRITE/" suffix))))
    `(progn
       (declaim (ftype (function (octet-vector (integer 0))
                                 (,(if signed-p 'signed-byte 'unsigned-byte)
                                   ,nb-bits))
                       ,read-function)
                (inline ,read-function))
       (defun ,read-function (octets offset)
         (declare (type octet-vector octets)
                  (type (integer 0) offset))
         (let ((value 0))
           ,@(let (forms)
               (dotimes (i nb-octets (nreverse forms))
                 (push `(setf (ldb (byte 8 ,(if (eq endianness :big-endian)
                                                (* (- nb-octets i 1) 8)
                                                (* i 8)))
                                   value)
                              (aref octets (+ offset ,i)))
                       forms)))
           ,(if signed-p
                `(if (logbitp ,(1- nb-bits) value)
                     (- value ,(ash 1 nb-bits))
                     value)
                'value)))
       (declaim (ftype (function ((,(if signed-p 'signed-byte 'unsigned-byte)
                                    ,nb-bits)
                                  octet-vector (integer 0)))
                       ,write-function)
                (inline ,write-function))
       (defun ,write-function (value octets offset)
         (declare (type (,(if signed-p 'signed-byte 'unsigned-byte) ,nb-bits)
                        value)
                  (type octet-vector octets)
                  (type (integer 0) offset))
         (let ((unsigned-value ,(if signed-p
                                    `(if (< value 0)
                                         (+ value ,(ash 1 nb-bits))
                                         value)
                                    'value)))
           ,@(let (forms)
               (dotimes (i nb-octets (nreverse forms))
                 (push `(setf (aref octets (+ offset ,i))
                              (ldb (byte 8 ,(if (eq endianness :big-endian)
                                                (* (- nb-octets i 1) 8)
                                                (* i 8)))
                                   unsigned-value))
                       forms)))
           ,(when signed-p
              `(when (< value 0)
                 (setf (ldb (byte 1 7)
                            (aref octets ,(if (eq endianness :big-endian)
                                              `offset
                                              `(+ offset ,(1- nb-octets)))))
                       1)))
           value)))))

(defmacro define-binref-float-functions (type type-size endianness)
  (let* ((nb-octets type-size)
         (nb-bits (* nb-octets 8))
         (suffix (format nil "FLOAT~D~A" nb-bits
                         (case endianness
                           (:big-endian "BE")
                           (:little-endian "LE"))))
         (read-function
           (intern (concatenate 'string "BINREF-READ/" suffix)))
         (write-function
           (intern (concatenate 'string "BINREF-WRITE/" suffix))))
    `(progn
       (declaim (ftype (function (octet-vector (integer 0)) ,type)
                       ,read-function)
                (inline ,read-function))
       (defun ,read-function (octets offset)
         (declare (type octet-vector octets)
                  (type (integer 0) offset))
         ;; We have to copy the octet vector because we may have to write it
         ;; to swap and it can be read-only (e.g. #(1 2 3 4)).
         (let ((octets (subseq octets offset (+ offset ,type-size))))
           (unless (eq *endianness* ,endianness)
             (dotimes (i ,(/ type-size 2))
               (rotatef (aref octets (+ offset i))
                        (aref octets (+ offset (- ,type-size i 1))))))
           (read-binary-float octets offset ,type-size)))
       (declaim (ftype (function (,type octet-vector (integer 0)))
                       ,write-function)
                (inline ,write-function))
       (defun ,write-function (value octets offset)
         (declare (type ,type value)
                  (type octet-vector octets)
                  (type (integer 0) offset))
         (write-binary-float value octets offset ,type-size)
         (unless (eq *endianness* ,endianness)
           (dotimes (i ,(/ type-size 2))
             (rotatef (aref octets (+ offset i))
                      (aref octets (+ offset (- ,type-size i 1))))))
         value))))

(declaim (inline read-binary-float))
(defun read-binary-float (octets offset size)
  (declare (type octet-vector octets)
           (type (integer 0) offset)
           (type (member 4 8) size))
  #+sbcl
  (sb-sys:with-pinned-objects (octets)
    (let ((%octets (sb-sys:vector-sap octets)))
      (ecase size
        (4 (sb-sys:sap-ref-single %octets offset))
        (8 (sb-sys:sap-ref-double %octets offset)))))
  #+ccl
  (ccl:with-pointer-to-ivector (%octets octets)
    (ecase size
      (4 (ccl::%get-single-float %octets offset))
      (8 (ccl::%get-double-float %octets offset))))
  #-(or sbcl ccl)
  (unsupported-feature "binary float reading"))

(declaim (inline write-binary-float))
(defun write-binary-float (value octets offset size)
  (declare (type float value)
           (type octet-vector octets)
           (type (integer 0) offset)
           (type (member 4 8) size))
  #+sbcl
  (sb-sys:with-pinned-objects (octets)
    (let ((%octets (sb-sys:vector-sap octets)))
      (ecase size
        (4 (setf (sb-sys:sap-ref-single %octets offset) value))
        (8 (setf (sb-sys:sap-ref-double %octets offset) value)))))
  #+ccl
  (ccl:with-pointer-to-ivector (%octets octets)
    (ecase size
      (4 (ccl::%set-single-float %octets offset value))
      (8 (ccl::%set-double-float %octets offset value))))
  #-(or sbcl ccl)
  (unsupported-feature "binary float writing"))

(define-binref-integer-functions 1 t nil)
(define-binref-integer-functions 1 nil nil)
(define-binref-integer-functions 2 t :big-endian)
(define-binref-integer-functions 2 nil :big-endian)
(define-binref-integer-functions 2 t :little-endian)
(define-binref-integer-functions 2 nil :little-endian)
(define-binref-integer-functions 4 t :big-endian)
(define-binref-integer-functions 4 nil :big-endian)
(define-binref-integer-functions 4 t :little-endian)
(define-binref-integer-functions 4 nil :little-endian)
(define-binref-integer-functions 8 t :big-endian)
(define-binref-integer-functions 8 nil :big-endian)
(define-binref-integer-functions 8 t :little-endian)
(define-binref-integer-functions 8 nil :little-endian)

(define-binref-float-functions single-float 4 :big-endian)
(define-binref-float-functions single-float 4 :little-endian)
(define-binref-float-functions double-float 8 :big-endian)
(define-binref-float-functions double-float 8 :little-endian)

(defun binref-read-function (type)
  (case type
    (:int8      'binref-read/int8)
    (:uint8     'binref-read/uint8)
    (:int16be   'binref-read/int16be)
    (:int16le   'binref-read/int16le)
    (:uint16be  'binref-read/uint16be)
    (:uint16le  'binref-read/uint16le)
    (:int32be   'binref-read/int32be)
    (:int32le   'binref-read/int32le)
    (:uint32be  'binref-read/uint32be)
    (:uint32le  'binref-read/uint32le)
    (:int64be   'binref-read/int64be)
    (:int64le   'binref-read/int64le)
    (:uint64be  'binref-read/uint64be)
    (:uint64le  'binref-read/uint64le)
    (:float32be 'binref-read/float32be)
    (:float32le 'binref-read/float32le)
    (:float64be 'binref-read/float64be)
    (:float64le 'binref-read/float64le)
    (t (error 'unknown-binary-type :type type))))

(defun binref-write-function (type)
  (ecase type
    (:int8      'binref-write/int8)
    (:uint8     'binref-write/uint8)
    (:int16be   'binref-write/int16be)
    (:int16le   'binref-write/int16le)
    (:uint16be  'binref-write/uint16be)
    (:uint16le  'binref-write/uint16le)
    (:int32be   'binref-write/int32be)
    (:int32le   'binref-write/int32le)
    (:uint32be  'binref-write/uint32be)
    (:uint32le  'binref-write/uint32le)
    (:int64be   'binref-write/int64be)
    (:int64le   'binref-write/int64le)
    (:uint64be  'binref-write/uint64be)
    (:uint64le  'binref-write/uint64le)
    (:float32be 'binref-write/float32be)
    (:float32le 'binref-write/float32le)
    (:float64be 'binref-write/float64be)
    (:float64le 'binref-write/float64le)
    (t (error 'unknown-binary-type :type type))))

(defun binref-type-size (type)
  (ecase type
    ((:int8 :uint8)
     1)
    ((:int16be :int16le :uint16be :uint16le)
     2)
    ((:int32be :int32le :uint32be :uint32le :float32be :float32le)
     4)
    ((:int64be :int64le :uint64be :uint64le :float64be :float64le)
     8)
    (t
     (error 'unknown-binary-type :type type))))

(defun binref (type octets &optional (offset 0))
  (declare (type keyword type)
           (type octet-vector octets))
  (funcall (binref-read-function type) octets offset))

(define-compiler-macro binref (&whole form type octets &optional (offset 0))
  (if (constantp type)
      (let ((octets-var (gensym "OCTETS-"))
            (offset-var (gensym "OFFSET-")))
        `(let* ((,octets-var ,octets)
                (,offset-var ,offset))
           (,(binref-read-function type) ,octets-var ,offset-var)))
      form))

(defsetf binref (type octets &optional (offset 0)) (value)
  (if (constantp type)
      (let ((octets-var (gensym "OCTETS-"))
            (offset-var (gensym "OFFSET-")))
        `(let* ((,octets-var ,octets)
                (,offset-var ,offset))
           (,(binref-write-function type) ,value ,octets-var ,offset-var)))
      `(funcall (binref-write-function ,type) ,value ,octets ,offset)))
