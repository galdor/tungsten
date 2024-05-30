(in-package :streams)

(defclass octet-input-stream (streams:fundamental-binary-input-stream
                              streams:fundamental-character-input-stream)
  ((data
    :type core:octet-vector
    :initarg :data)
   (start
    :type (integer 0)
    :initarg :start)
   (end
    :type (integer 0)
    :initarg :end)
   (external-format
    :type text:external-format
    :initarg :external-format
    :initform text:*default-external-format*
    :reader octet-input-stream-external-format)
   (last-start
    :type (or (integer 0) null)
    :initform nil)))

(defun make-octet-input-stream (data &key (start 0) end
                                          (external-format
                                           text:*default-external-format*))
  (declare (type core:octet-vector data)
           (type (integer 0) start)
           (type (or (integer 0) null) end)
           (type text:external-format external-format))
  (make-instance 'octet-input-stream :data data
                                     :start start :end (or end (length data))
                                     :external-format external-format))

(defmacro with-input-from-octet-vector ((stream data &key (start 0) end
                                                          external-format)
                                        &body body)
  `(let ((,stream
           (make-octet-input-stream ,data :start ,start :end ,end
                                    ,@(when external-format
                                        `(:external-format ,external-format)))))
     ,@body))

(defun octet-input-stream-decode-character (stream)
  (declare (type octet-input-stream stream))
  (with-slots (data start end external-format) stream
    (let ((encoding (text:external-format-encoding external-format)))
      (text:decode-character data :start start :end end
                                  :encoding encoding))))

(defmethod stream-read-char ((stream octet-input-stream))
  (with-slots (data start end external-format last-start) stream
    (multiple-value-bind (c size)
        (octet-input-stream-decode-character stream)
      (cond
        (c
         (setf last-start start)
         (incf start size)
         c)
        (t
         :eof)))))

(defmethod stream-unread-char ((stream octet-input-stream) c)
  (declare (type character c))
  (with-slots (start last-start) stream
    (unless last-start
      (error "cannot unread a character without having read a character"))
    (setf start last-start)
    (let ((c2 (stream-peek-char stream)))
      (when (char/= c c2)
        (error "cannot unread ~S after having read ~S" c c2)))
    nil))

(defmethod stream-read-char-no-hang ((stream octet-input-stream))
  (stream-read-char stream))

(defmethod stream-peek-char ((stream octet-input-stream))
  (or (octet-input-stream-decode-character stream) :eof))

(defmethod stream-listen ((stream octet-input-stream))
  (let ((c (stream-read-char-no-hang stream)))
    (when (and c (not (eq c :eof)))
      (stream-unread-char stream c)
      t)))

(defmethod stream-read-line ((stream octet-input-stream))
  (with-slots (last-start) stream
    (setf last-start nil)
    ;; TODO
    nil))

(defmethod stream-read-line ((stream octet-input-stream))
  (let* ((line (make-array 0 :element-type 'character
                             :adjustable t :fill-pointer 0))
         (eol (text:eol-string
               (text:external-format-eol-style
                (octet-input-stream-external-format stream))))
         (eol-buffer (make-string (length eol))))
    (loop
      (dotimes (i (length eol))
        (let ((c (stream-read-char stream)))
          (cond
            ((eq c :eof)
             (dotimes (j i)
               (vector-push-extend (char eol-buffer j) line))
             (return-from stream-read-line (values line t)))
            ((char= c (char eol i))
             (setf (char eol-buffer i) c)
             (when (= i (1- (length eol)))
               (return-from stream-read-line (values line nil))))
            (t
             (dotimes (j i)
               (vector-push-extend (char eol-buffer j) line))
             (vector-push-extend c line)
             (return))))))))

(defmethod stream-clear-input ((stream octet-input-stream))
  (with-slots (start end last-start) stream
    (setf last-start nil)
    (setf start end)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (with-slots (data start end last-start) stream
    (setf last-start nil)
    (if (< start end)
        (prog1 (aref data start)
          (incf start))
        :eof)))

(defmethod stream-read-sequence ((stream octet-input-stream) octets
                                 &optional (start 0) end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (let ((end (or end (length octets))))
    (with-slots (data (data-start start) (data-end end) last-start) stream
      (setf last-start nil)
      (replace octets data :start1 start :end1 end
                           :start2 data-start :end2 data-end)
      (let ((nb-read (min (- end start) (- data-end data-start))))
        (incf data-start nb-read)
        (+ start nb-read)))))
