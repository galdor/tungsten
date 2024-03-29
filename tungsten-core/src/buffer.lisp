(in-package :core)

(defclass buffer ()
  ((data
    :type octet-vector
    :reader buffer-data)
   (start
    :type (integer 0)
    :initform 0
    :accessor buffer-start)
   (end
    :type (integer 0)
    :initform 0
    :accessor buffer-end))
  (:documentation
   "A buffer designed to store octets in a simple vector.
Optimized to be read at the start and written at the end."))

(defmethod initialize-instance :after ((buffer buffer)
                                       &key (initial-size 64)
                                       &allow-other-keys)
  (with-slots (data) buffer
    (setf data (make-array initial-size :element-type 'octet))))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t)
    (format stream "~D/~D" (buffer-length buffer) (buffer-capacity buffer))))

(defun make-buffer (initial-size)
  "Create and return a new empty buffer."
  (declare (type (integer 0) initial-size))
  (make-instance 'buffer :initial-size initial-size))

(defun buffer-empty-p (buffer)
  "Return T if buffer does not contain any data or NIL else."
  (declare (type buffer buffer))
  (with-slots (start end) buffer
    (= start end)))

(defun buffer-capacity (buffer)
  "Return the number of octets available in the buffer."
  (declare (type buffer buffer))
  (with-slots (data) buffer
    (length data)))

(defun buffer-length (buffer)
  "Return the number of octets stored in BUFFER."
  (declare (type buffer buffer))
  (with-slots (start end) buffer
    (- end start)))

(defun buffer-content (buffer)
  "Return a copy of the data stored in BUFFER."
  (declare (type buffer buffer))
  (with-slots (data start end) buffer
    (subseq data start end)))

(defun buffer-append-octet (buffer octet)
  "Add an octet to the end of BUFFER."
  (declare (type buffer buffer)
           (type octet octet))
  (with-slots (data end) buffer
    (setf (aref data (buffer-reserve buffer 1)) octet)
    (incf end)))

(defun buffer-append-octets (buffer octets
                             &key (start 0) (end (length octets)))
  "Copy an octet vector to the end of BUFFER."
  (declare (type buffer buffer)
           (type octet-vector octets))
  (let ((nb-octets (- end start)))
    (with-slots (data (buffer-end end)) buffer
      (let ((position (buffer-reserve buffer nb-octets)))
        (replace data octets :start1 position :start2 start :end2 end))
      (incf buffer-end nb-octets))))

(defun buffer-reserve (buffer n)
  "Alter BUFFER so that it has space for at least N additional octets and
return the position of the first free octet in the buffer."
  (declare (type buffer buffer)
           (type (integer 1) n))
  (with-slots (data start end) buffer
    (let ((end-space (- (length data) end))
          (start-space start))
      (cond
        ((<= n end-space)
         ;; There is already enough free space at the end
         nil)
        ((<= n (+ start-space end-space))
         ;; There is enough space if we remove unused space at the start
         (buffer-compact buffer))
        (t
         ;; We have to increase the length of the underlying array
         (buffer-compact buffer)
         (buffer-grow buffer (max (- n end) (* (length data) 2))))))
    end))

(defun buffer-reserve-start (buffer n)
  "Alter BUFFER so that it has space for N additional octets before its current
content and return the position of the first free octet in the buffer."
  (declare (type buffer buffer)
           (type (integer 1) n))
  (with-slots (data start end) buffer
    (let ((end-space (- (length data) end))
          (start-space start))
      (cond
        ((<= n start-space)
         ;; There is already enough free space at the beginning
         nil)
        ((<= n (+ start-space end-space))
         ;; There is enough space if we use some of unused space at the end
         (replace data data :start1 n :start2 start :end2 end)
         (setf end (+ n (- end start))
               start n))
        (t
         ;; We have to increase the length of the underlying array
         (buffer-compact buffer)
         (buffer-grow buffer (max (- n end) (* (length data) 2)))
         (replace data data :start1 n :end2 end)
         (setf end (+ n (- end start))
               start n)))
      (- start n))))

(defun buffer-grow (buffer n)
  "Resize BUFFER to increase its length by N octets."
  (declare (type buffer buffer)
           (type (integer 1) n))
  (with-slots (data start end) buffer
    (let ((data2 (make-array (+ (length data) n) :element-type 'octet)))
      (replace data2 data :start2 start :end2 end)
      (setf data data2
            end (- end start)
            start 0))))

(defun buffer-compact (buffer)
  "Alter BUFFER to remove any unused space at the start."
  (declare (type buffer buffer))
  (with-slots (data start end) buffer
    (when (> start 0)
      (replace data data :start2 start :end2 end)
      (setf end (- end start)
            start 0))))

(defun buffer-reset (buffer)
  "Delete all content from BUFFER."
  (with-slots (start end) buffer
    (setf start 0
          end 0)))

(defun buffer-skip (buffer n)
  "Remove N octets at the beginning of BUFFER."
  (declare (type buffer buffer)
           (type (integer 0) n))
  (with-slots (data start end) buffer
    (when (> n (- end start))
      (error "cannot skip ~D octets in a buffer containing ~D octet~:P"
             n (- end start)))
    (incf start n)
    (when (= start end)
      (setf start 0
            end 0))))

(defun buffer-skip-to (buffer position)
  "Remove all octets before POSITION in BUFFER."
  (declare (type buffer buffer)
           (type (integer 0) position))
  (with-slots (data start end) buffer
    (setf start position)
    (when (= start end)
      (setf start 0
            end 0))))

(defun buffer-starts-with-octet (buffer octet)
  (declare (type buffer buffer)
           (type octet octet))
  (with-slots (data start end) buffer
    (and (> (- end start) 0)
         (= (aref data start) octet))))

(defun buffer-starts-with-octets (buffer octets)
  (declare (type buffer buffer)
           (type octet-vector octets))
  (with-slots (data start end) buffer
    (let ((nb-octets (length octets)))
      (and (>= (- end start) nb-octets)
           (do ((i start (1+ i))
                (j 0 (1+ j)))
               ((>= j nb-octets)
                t)
             (when (/= (aref data i) (aref octets j))
               (return nil)))))))
