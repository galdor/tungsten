(in-package :uuid)

(deftype version ()
  '(member :v4 :v7))

(defstruct (uuid
            (:constructor nil))
  (octets #.(core:octet-vector* 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
   :type (core:octet-vector 16)))

(defmethod print-object ((id uuid) stream)
  (print-unreadable-object (id stream :type t)
    (write-string (serialize id) stream)))

(defun uuid (data)
  (declare (type (or string (core:octet-vector 16)) data))
  (etypecase data
    (string
     (parse data))
    ((core:octet-vector 16)
     (let ((id (make-instance 'uuid)))
       (setf (slot-value id 'octets) data)
       id))))

(defun uuid-equal (id1 id2)
  (equalp (uuid-octets id1) (uuid-octets id2)))

(defun generate (version)
  (declare (type version version))
  (let ((octets (core:random-octets 16)))
    (ecase version
      (:v4
       (setf (ldb (byte 8 4) (aref octets 6)) 4)
       (setf (ldb (byte 8 6) (aref octets 8)) 2))
      (:v7
       (let ((timestamp (time:datetime-unix-timestamp
                         (time:current-datetime)
                         :unit :millisecond))
             (timestamp-octets (core:make-octet-vector 8)))
         (setf (core:binref :uint64be timestamp-octets) timestamp)
         (replace octets timestamp-octets :end1 6 :start2 2))
       (setf (ldb (byte 8 4) (aref octets 6)) 7)
       (setf (ldb (byte 8 6) (aref octets 8)) 2)))
    (uuid octets)))

(define-condition invalid-format (error)
  ((string
    :type string
    :initarg :string
    :reader invalid-format-string))
  (:report
   (lambda (condition stream)
     (format stream "invalid UUID ~S" (invalid-format-string condition)))))

(defun parse (string &key (start 0) end)
  (declare (type string string)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (let ((end (or end (length string))))
    (unless (= (- end start) 36)
      (error 'invalid-format :string (subseq string start end)))
    (unless (and (char= (char string (+ start 8)) #\-)
                 (char= (char string (+ start 13)) #\-)
                 (char= (char string (+ start 18)) #\-)
                 (char= (char string (+ start 23)) #\-))
      (error 'invalid-format :string (subseq string start end)))
    (handler-case
        (let ((octets (core:make-octet-vector 16)))
          (flet ((decode (start2 end2)
                   (declare (type integer start2 end2))
                   (text:decode-hex-string
                    string :start (+ start start2) :end (+ start end2))))
            (replace octets (decode 0 8) :start1 0 :end1 4)
            (replace octets (decode 9 13) :start1 4 :end1 6)
            (replace octets (decode 14 18) :start1 6 :end1 8)
            (replace octets (decode 19 23) :start1 8 :end1 10)
            (replace octets (decode 24 36) :start1 10 :end1 16))
          (uuid octets))
      ((or text:invalid-hex-string text:invalid-hex-digit) ()
        (error 'invalid-format :string (subseq string start end))))))

(defun serialize (id)
  (declare (type uuid id))
  (with-slots (octets) id
    (concatenate
     'string
     (text:encode-hex-string octets :start 0 :end 4)
     "-"
     (text:encode-hex-string octets :start 4 :end 6)
     "-"
     (text:encode-hex-string octets :start 6 :end 8)
     "-"
     (text:encode-hex-string octets :start 8 :end 10)
     "-"
     (text:encode-hex-string octets :start 10 :end 16))))
