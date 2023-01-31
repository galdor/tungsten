(in-package :postgresql)

(deftype codec-type ()
  '(or symbol list))

(deftype oid ()
  '(unsigned-byte 32))

(defparameter *date-day-base* 10957
  "The number of days since the UNIX epoch used as base date for PostgreSQL
dates. Corresponds to 2000-01-01.")

(defparameter *timestamp-microsecond-base*
  (* *date-day-base* 86400 1000000)
  "The microsecond UNIX timestamp used as base date for PostgreSQL timestamps.
Corresponds to 2000-01-01T00:00:00.")

(define-condition unknown-codec (error)
  ((type
    :type (or codec-type null)
    :initarg :type
    :initform nil
    :reader unknown-codec-type)
   (oid
    :type (or oid null)
    :initarg :oid
    :initform nil
    :reader unknown-codec-oid))
  (:report
   (lambda (condition stream)
     (with-slots (type oid) condition
       (if type
           (format stream "No codec found for type ~S." type)
           (format stream "No codec found for type OID ~D." oid))))))

(define-condition unencodable-value (error)
  ((value
    :initarg :value
    :reader unencodable-value-value))
  (:report
   (lambda (condition stream)
     (with-slots (value) condition
       (format stream "Value ~S cannot be encoded to a PostgreSQL value."
               value)))))

(define-condition value-decoding-error (simple-error)
  ((octets
    :type core:octet-vector
    :initarg :octets))
  (:report
   (lambda (condition stream)
     (format stream "Cannot decode value: ~?."
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))

(defun value-decoding-error (octets format &rest arguments)
  (error 'value-decoding-error :octets octets
                               :format-control format
                               :format-arguments arguments))

(defun make-codec-table ()
  (make-hash-table :test #'equal))

(defclass codec ()
  ((type
    :type codec-type
    :initarg :type
    :reader codec-type)
   (oid
    :type oid
    :initarg :oid
    :reader codec-oid)
   (encoding-function
    :type (or symbol function)
    :initarg :encoding-function
    :reader codec-encoding-function)
   (decoding-function
    :type (or symbol function)
    :initarg :decoding-function
    :reader codec-decoding-function)))

(defun make-codec (type oid encoding-function decoding-function)
  (declare (type codec-type type)
           (type oid oid)
           (type (or symbol function) encoding-function decoding-function))
  (make-instance 'codec :type type
                        :oid oid
                        :encoding-function encoding-function
                        :decoding-function decoding-function))

(defmethod print-object ((codec codec) stream)
  (print-unreadable-object (codec stream :type t)
    (with-slots (type oid) codec
      (format stream "~A ~D" type oid))))

(defun find-codec (codec-designator codecs)
  (declare (type (or codec codec-type oid) codec-designator)
           (type hash-table codecs))
  (etypecase codec-designator
    (codec
     codec-designator)
    (codec-type
     (or (gethash codec-designator codecs)
         (error 'unknown-codec :type codec-designator)))
    (oid
     (or (gethash codec-designator codecs)
         (error 'unknown-codec :oid codec-designator)))))

(defun register-codec (codec codecs)
  (declare (type codec codec)
           (type hash-table codecs))
  (with-slots (type oid) codec
    (delete-codec type codecs)
    (delete-codec oid codecs)
    (setf (gethash type codecs) codec
          (gethash oid codecs) codec)))

(defun delete-codec (type-or-oid codecs)
  (declare (type (or codec-type oid) type-or-oid)
           (type hash-table codecs))
  (let ((codec (gethash type-or-oid codecs)))
    (when codec
      (remhash (codec-type codec) codecs)
      (remhash (codec-oid codec) codecs)
      t)))

(defun encode-value (value codecs)
  (declare (type hash-table codecs))
  (cond
    ((null value)
     '(0 . nil))
    ((eq value :void)
     (encode-value '(:void . nil) codecs))
    ((eq value :true)
     (encode-value '(:boolean . :true) codecs))
    ((eq value :false)
     (encode-value '(:boolean . :false) codecs))
    ((typep value '(signed-byte 16))
     (encode-value `(:int2 . ,value) codecs))
    ((typep value '(signed-byte 32))
     (encode-value `(:int4 . ,value) codecs))
    ((typep value '(signed-byte 64))
     (encode-value `(:int8 . ,value) codecs))
    ((typep value 'single-float)
     (encode-value `(:float4 . ,value) codecs))
    ((typep value 'double-float)
     (encode-value `(:float8 . ,value) codecs))
    ((typep value 'string)
     (encode-value `(:text . ,value) codecs))
    ((typep value 'core:octet-vector)
     (encode-value `(:bytea . ,value) codecs))
    ((typep value 'time:datetime)
     (encode-value `(:timestamp . ,value) codecs))
    ((consp value)
     (let* ((codec (find-codec (car value) codecs))
            (value (cdr value)))
       (cons (codec-oid codec)
             (when value
               (funcall (codec-encoding-function codec) value codec codecs)))))
    (t
     (error 'unencodable-value :value value))))

(defun decode-value (octets codec-designator codecs)
  (declare (type (or core:octet-vector null) octets)
           (type (or codec codec-type oid) codec-designator)
           (type hash-table codecs))
  (when octets
    (let ((codec (find-codec codec-designator codecs)))
      (funcall (codec-decoding-function codec) octets codecs))))

(defun make-default-codec-table ()
  (let ((codecs (make-codec-table)))
    (mapcar
     (lambda (entry)
       (destructuring-bind (type value-oid array-oid
                            encoding-function decoding-function)
           entry
         (register-codec (make-codec type value-oid
                                     encoding-function decoding-function)
                         codecs)
         (register-codec (make-codec (list :array type) array-oid
                                     'encode-value/array 'decode-value/array)
                         codecs)))
     `((:boolean
        16 1000
        encode-value/boolean decode-value/boolean)
       (:bytea
        17 1001
        encode-value/bytea decode-value/bytea)
       (:name
        19 1003
        encode-value/text decode-value/text)
       (:int8
        20 1016
        ,(integer-value-encoding-function 8 :int64be)
        ,(integer-value-decoding-function 8 :int64be))
       (:int2
        21 1005
        ,(integer-value-encoding-function 2 :int16be)
        ,(integer-value-decoding-function 2 :int16be))
       (:int4
        23 1007
        ,(integer-value-encoding-function 4 :int32be)
        ,(integer-value-decoding-function 4 :int32be))
       (:text
        25 1009
        encode-value/text decode-value/text)
       (:oid
        26 1028
        ,(integer-value-encoding-function 4 :uint32be)
        ,(integer-value-decoding-function 4 :uint32be))
       (:float4
        700 1021
        ,(floating-point-value-encoding-function 4 :float32be)
        ,(floating-point-value-decoding-function 4 :float32be))
       (:float8
        701 1022
        ,(floating-point-value-encoding-function 8 :float64be)
        ,(floating-point-value-decoding-function 8 :float64be))
       (:bpchar
        1042 1014
        encode-value/text decode-value/text)
       (:varchar
        1043 1015
        encode-value/text decode-value/text)
       (:date
        1082 1182
        encode-value/date decode-value/date)
       (:timestamp
        1114 1115
        encode-value/timestamp decode-value/timestamp)
       (:timestamptz
        1184 1185
        encode-value/timestamp decode-value/timestamp)))
    codecs))

(defun encode-value/array (value codec codecs)
  (let* ((element-type (cadr (codec-type codec)))
         (element-codec (find-codec element-type codecs))
         (rank (array-rank value))
         (array-contains-null-p
           (dotimes (i (array-total-size value) nil)
             (when (null (row-major-aref value i))
               (return t))))
         (flags (if array-contains-null-p 1 0)))
    (streams:with-output-to-octet-vector (stream)
      (let ((header (core:make-octet-vector (+ 12 (* rank 8))))
            (offset 0))
        (setf (core:binref :int32be header 0) rank
              (core:binref :int32be header 4) flags
              (core:binref :int32be header 8) (codec-oid element-codec))
        (incf offset 12)
        (dotimes (i rank)
          (setf (core:binref :int32be header offset) (array-dimension value i)
                (core:binref :int32be header (+ offset 4)) 1)
          (incf offset 8))
        (write-sequence header stream))
      (dotimes (i (array-total-size value))
        (let ((element (row-major-aref value i)))
          (cond
            ((null element)
             (let ((octets (core:make-octet-vector 4)))
               (setf (core:binref :int32be octets) -1)
               (write-sequence octets stream)))
            (t
             (let ((element-octets
                     (cdr (encode-value (cons element-type element) codecs)))
                   (length-octets (core:make-octet-vector 4)))
               (setf (core:binref :int32be length-octets)
                     (length element-octets))
               (write-sequence length-octets stream)
               (write-sequence element-octets stream)))))))))

(defun decode-value/array (octets codecs)
  (let ((nb-octets (length octets)))
    (when (< nb-octets 12)
      (value-decoding-error octets "truncated array header"))
    (let* ((rank (core:binref :int32be octets 0))
           (element-oid (core:binref :int32be octets 8))
           (element-codec (find-codec element-oid codecs))
           (dimensions nil)
           (offset 12))
      (decf nb-octets 12)
      (unless (>= nb-octets (* rank 8))
        (value-decoding-error octets "truncated array dimensions"))
      (dotimes (i rank)
        (push (core:binref :int32be octets offset) dimensions)
        (incf offset 8)
        (decf nb-octets 8))
      (let ((elements (make-array (reverse dimensions)))
            (nb-elements (reduce #'* dimensions)))
        (dotimes (i nb-elements elements)
          (when (< nb-octets 4)
            (value-decoding-error octets "truncated array element size"))
          (let ((size (core:binref :int32be octets offset)))
            (incf offset 4)
            (decf nb-octets 4)
            (cond
              ((= size -1)
               (setf (row-major-aref elements i) nil))
              (t
               (when (< nb-octets size)
                 (value-decoding-error octets "truncated array element"))
               ;; It would be nice to have all value decoding functions
               ;; accept OCTETS, START and END to avoid allocation here.
               (let ((element-octets (subseq octets offset (+ offset size))))
                 (setf (row-major-aref elements i)
                       (decode-value element-octets element-codec codecs)))
               (incf offset size)
               (decf nb-octets size)))))))))

(defun encode-value/boolean (value codec codecs)
  (declare (type keyword value)
           (ignore codec codecs))
  (ecase value
    (:true
     (core:octet-vector* 1))
    (:false
     (core:octet-vector* 0))))

(defun decode-value/boolean (octets codecs)
  (declare (ignore codecs))
  (unless (= (length octets) 1)
    (value-decoding-error octets "boolean is not 1 byte long"))
  (case (aref octets 0)
    (0 :false)
    (t :true)))

(defun encode-value/bytea (value codec codecs)
  (declare (type core:octet-vector value)
           (ignore codec codecs))
  value)

(defun decode-value/bytea (octets codecs)
  (declare (ignore codecs))
  octets)

(defun integer-value-encoding-function (size type)
  (lambda (value codec codecs)
    (declare (type integer value)
             (ignore codec codecs))
    (let ((octets (core:make-octet-vector size)))
      (setf (core:binref type octets) value)
      octets)))

(defun integer-value-decoding-function (size type)
  (lambda (octets codecs)
    (declare (ignore codecs))
    (when (< (length octets) size)
      (value-decoding-error octets "truncated ~D byte integer" size))
    (core:binref type octets)))

(defun floating-point-value-encoding-function (size type)
  (lambda (value codec codecs)
    (declare (type float value)
             (ignore codec codecs))
    (let ((octets (core:make-octet-vector size)))
      (setf (core:binref type octets) value)
      octets)))

(defun floating-point-value-decoding-function (size type)
  (lambda (octets codecs)
    (declare (ignore codecs))
    (when (< (length octets) size)
      (value-decoding-error octets "truncated ~D byte floating point value"
                            size))
    (core:binref type octets)))

(defun encode-value/text (value codec codecs)
  (declare (type string value)
           (ignore codec codecs))
  (text:encode-string value))

(defun decode-value/text (octets codecs)
  (declare (ignore codecs))
  (text:decode-string octets))

(defun encode-value/date (value codec codecs)
  (declare (type time:datetime value)
           (ignore codec codecs))
  (let* ((timestamp (time:datetime-unix-timestamp value :unit :second))
         (nb-days (floor timestamp 86400)))
    (let ((octets (core:make-octet-vector 4)))
      (setf (core:binref :int32be octets) (- nb-days *date-day-base*))
      octets)))

(defun decode-value/date (octets codecs)
  (declare (ignore codecs))
  (when (< (length octets) 4)
    (value-decoding-error octets "truncated date value"))
  (let ((nb-days (+ (core:binref :int32be octets) *date-day-base*)))
    (time:make-datetime-from-unix-timestamp (* nb-days 86400))))

(defun encode-value/timestamp (value codec codecs)
  (declare (type time:datetime value)
           (ignore codec codecs))
  (let ((timestamp (time:datetime-unix-timestamp value :unit :microsecond)))
    (let ((octets (core:make-octet-vector 8)))
      (setf (core:binref :int64be octets)
            (- timestamp *timestamp-microsecond-base*))
      octets)))

(defun decode-value/timestamp (octets codecs)
  (declare (ignore codecs))
  (when (< (length octets) 8)
    (value-decoding-error octets "truncated timestamp value"))
  (let ((timestamp (core:binref :int64be octets)))
    (time:make-datetime-from-unix-timestamp
     (+ timestamp *timestamp-microsecond-base*)
     :unit :microsecond)))
