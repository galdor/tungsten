(in-package :imf)

(defvar *line-writer* nil)

(defclass line-writer ()
  ((max-line-length
    :type (integer 1)
    :initarg :max-line-length
    :reader line-writer-max-line-length)
   (line
    :type string)
   (force-fold
    :type boolean
    :initform nil)
   (stream
    :type stream
    :initarg :stream
    :reader line-writer-stream)))

(defmethod initialize-instance :after ((writer line-writer)
                                       &key &allow-other-keys)
  (with-slots (max-line-length line) writer
    (setf line (make-array max-line-length :element-type 'character
                                           :adjustable t :fill-pointer 0))))

(defun make-line-writer (stream &key (max-line-length 76))
  (declare (type stream stream)
           (type (integer 1) max-line-length))
  (make-instance 'line-writer :max-line-length max-line-length
                              :stream stream))

(defmacro with-line-writer ((stream &rest args) &body body)
  `(let ((*line-writer* (make-line-writer ,stream ,@args)))
     ,@body
     (line-writer-flush *line-writer*)))

(defmacro with-line-writer/string ((&rest args) &body body)
  (let ((stream (gensym "STREAM-")))
    `(with-output-to-string (,stream)
       (with-line-writer (,stream ,@args)
         ,@body))))

(defun write-token (token &optional (writer *line-writer*))
  (declare (type (or string character (member :space :eol)) token)
           (type line-writer writer))
  (with-slots (line force-fold stream) writer
    (flet ((append-string (string)
             (when (> (fill-pointer line) 0)
               (let* ((capacity (- (array-dimension line 0)
                                   (fill-pointer line))))
                 (when (or force-fold (>= (length string) capacity))
                   ;; If the string does not fit in the current line, we have
                   ;; to fold it.
                   (line-writer-flush writer)
                   (write-string (text:eol-string :crlf) stream)
                   (write-char #\Space stream)
                   (setf force-fold nil))))
             (cond
               ((< (length string) (array-dimension line 0))
                ;; If the string fits in the line buffer, we just append it
                (let ((start1 (fill-pointer line)))
                  (incf (fill-pointer line) (length string))
                  (replace line string :start1 start1)))
               (t
                ;; If the string is longer than the line limit, we have no
                ;; choice but to write it directly to the output stream and
                ;; too bad for the line length limit. We also set the
                ;; FORCE-FOLD flag to make sure the next token is written on
                ;; the next line.
                (write-string string stream)
                (setf force-fold t)))))
      (cond
        ((eq token :eol)
         (line-writer-flush writer)
         (write-string (text:eol-string :crlf) stream))
        ((eq token :space)
         (append-string " "))
        (t
         (append-string (string token)))))))

(defun line-writer-flush (writer)
  (declare (type line-writer writer))
  (with-slots (line stream) writer
    (unless (zerop (fill-pointer line))
      ;; If there was a trailing whitespace, we don't need it
      ;; anymore since we'll have one in the next line
      ;; continuation.
      (when (char= (char line (1- (length line))) #\Space)
        (decf (fill-pointer line)))
      (write-string line stream)
      (setf (fill-pointer line) 0))))
