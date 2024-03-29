(in-package :imf)

(defvar *line-writer* nil)

(defgeneric write-tokens (object))

(defclass line-writer ()
  ((max-line-length
    :type (or (integer 1) null)
    :initarg :max-line-length
    :initform nil
    :reader line-writer-max-line-length)
   (smtp
    :type boolean
    :initarg :smtp
    :reader line-writer-smtp)
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
    (when max-line-length
      (setf line (make-array max-line-length :element-type 'character
                                             :adjustable t :fill-pointer 0)))))

(defun make-line-writer (stream &key max-line-length smtp)
  (declare (type stream stream)
           (type (or (integer 1) null) max-line-length)
           (type boolean smtp))
  (make-instance 'line-writer :max-line-length max-line-length
                              :smtp smtp
                              :stream stream))

(defmacro with-line-writer ((stream &rest args) &body body)
  (let ((stream-var (gensym "STREAM-"))
        (output-stream (gensym "OUTPUT-STREAM-")))
    `(let* ((,stream-var ,stream)
            (,output-stream (if ,stream-var
                                ,stream-var
                                (make-string-output-stream))))
       (unwind-protect
            (let ((*line-writer* (make-line-writer ,output-stream ,@args)))
              ,@body
              (line-writer-flush *line-writer*)
              (unless ,stream-var
                (get-output-stream-string ,output-stream)))
         (unless ,stream-var
           (close ,output-stream))))))

(defun write-token (token &key (writer *line-writer*) no-folding)
  (declare (type (or string character (member :space :eol)) token)
           (type line-writer writer)
           (type boolean no-folding))
  (with-slots (max-line-length line force-fold stream) writer
    (cond
      (max-line-length
       (flet ((append-string (string)
                (when (and (> (fill-pointer line) 0)
                           (not no-folding))
                  (let* ((capacity (- (array-dimension line 0)
                                      (fill-pointer line))))
                    (when (or force-fold (>= (length string) capacity))
                      ;; If the string does not fit in the current line, we
                      ;; have to fold it.
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
                   ;; FORCE-FOLD flag to make sure the next token is written
                   ;; on the next line.
                   (write-string string stream)
                   (setf force-fold t)))))
         (cond
           ((eq token :eol)
            (line-writer-flush writer)
            (write-string (text:eol-string :crlf) stream))
           ((eq token :space)
            (append-string " "))
           (t
            (append-string (string token))))))
      (t
       ;; If there is no line length limit, we don't need the line buffer and
       ;; just write to the stream.
       (cond
         ((eq token :eol)
          (write-string (text:eol-string :crlf) stream))
         ((eq token :space)
          (write-char #\Space stream))
         (t
          (write-string (string token) stream)))))))

(defun line-writer-flush (writer)
  (declare (type line-writer writer))
  (when (slot-boundp writer 'line)
    (with-slots (line stream smtp) writer
      (unless (zerop (fill-pointer line))
        (when (and smtp (char= (char line 0) #\.))
          (write-char #\.))
        ;; If there was a trailing whitespace, we don't need it anymore since
        ;; we'll have one in the next line continuation.
        (when (char= (char line (1- (length line))) #\Space)
          (decf (fill-pointer line)))
        (write-string line stream)
        (setf (fill-pointer line) 0)))))
