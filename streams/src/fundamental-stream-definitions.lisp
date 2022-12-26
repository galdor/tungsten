(in-package :streams)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *gray-package*
    #+sbcl "SB-GRAY"
    #+ccl "CCL"))

(defmacro define-stream-class (name)
  `(setf (find-class ',name)
         (find-class
          (or (find-symbol ,(symbol-name name) *gray-package*)
              (error "symbol ~S not found in package ~S"
                     ,(symbol-name name) *gray-package*)))))

(define-stream-class fundamental-stream)
(define-stream-class fundamental-input-stream)
(define-stream-class fundamental-output-stream)
(define-stream-class fundamental-binary-stream)
(define-stream-class fundamental-binary-input-stream)
(define-stream-class fundamental-binary-output-stream)
(define-stream-class fundamental-character-stream)
(define-stream-class fundamental-character-input-stream)
(define-stream-class fundamental-character-output-stream)

(defmacro define-stream-generic (name &optional (original-name name))
  `(setf (fdefinition ',name)
         (fdefinition
          (or (find-symbol ,(symbol-name original-name) *gray-package*)
              (error "symbol ~S not found in package ~S"
                     ,(symbol-name original-name) *gray-package*)))))

;; Original generics from the Gray Streams document
(define-stream-generic stream-read-char)
(define-stream-generic stream-unread-char)
(define-stream-generic stream-read-char-no-hang)
(define-stream-generic stream-peek-char)
(define-stream-generic stream-listen)
(define-stream-generic stream-read-line)
(define-stream-generic stream-clear-input)
(define-stream-generic stream-read-byte)
(define-stream-generic stream-write-char)
(define-stream-generic stream-line-column)
(define-stream-generic stream-start-line-p)
(define-stream-generic stream-write-string)
(define-stream-generic stream-terpri)
(define-stream-generic stream-fresh-line)
(define-stream-generic stream-finish-output)
(define-stream-generic stream-force-output)
(define-stream-generic stream-clear-output)
(define-stream-generic stream-advance-to-column)
(define-stream-generic stream-write-byte)

;; Extensions
#-ccl
(progn
  (define-stream-generic stream-read-sequence)
  (define-stream-generic stream-write-sequence))

(define-stream-generic stream-file-position
  #+ccl stream-position)

#+ccl
(progn
  (defgeneric stream-read-sequence (stream sequence &optional start end))

  (defmethod stream-read-sequence ((stream fundamental-binary-input-stream)
                                   sequence
                                   &optional (start 0)
                                             (end (length sequence)))
      (do ((i start (1+ i)))
          ((>= i end)
           i)
        (let ((byte (read-byte stream nil :eof)))
          (if (eq byte :eof)
              (return-from stream-read-sequence i)
              (setf (aref sequence i) byte)))))

  (defmethod stream-read-sequence ((stream fundamental-character-input-stream)
                                   sequence
                                   &optional (start 0)
                                             (end (length sequence)))
      (do ((i start (1+ i)))
          ((>= i end)
           i)
        (let ((c (read-char stream nil :eof)))
          (if (eq c :eof)
              (return-from stream-read-sequence i)
              (setf (aref sequence i) c)))))

  (defgeneric stream-write-sequence (stream sequence &optional start end))

  (defmethod stream-write-sequence ((stream fundamental-binary-output-stream)
                                    sequence
                                    &optional (start 0)
                                              (end (length sequence)))
    (do ((i start (1+ i)))
        ((>= i end)
         sequence)
      (write-byte (aref sequence i) stream)))

  (defmethod stream-write-sequence ((stream
                                     fundamental-character-output-stream)
                                    sequence
                                    &optional (start 0)
                                              (end (length sequence)))
      (do ((i start (1+ i)))
          ((>= i end)
           sequence)
        (write-char (aref sequence i) stream)))

  (defmethod ccl:stream-read-vector ((stream fundamental-input-stream)
                                     sequence start end)
    (stream-read-sequence stream sequence start end))

  (defmethod ccl:stream-write-vector ((stream fundamental-output-stream)
                                      sequence start end)
    (stream-write-sequence stream sequence start end)))
