(in-package :streams)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *gray-package*
    #+sbcl "SB-GRAY"
    #+ccl "CCL"))

(defmacro define-stream-class (name)
  `(setf (find-class ',name)
         (find-class (or (find-symbol ,(symbol-name name) *gray-package*)
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

(defmacro define-stream-generic (name)
  `(setf (fdefinition ',name)
         (fdefinition (or (find-symbol ,(symbol-name name) *gray-package*)
                          (error "symbol ~S not found in package ~S"
                                 ,(symbol-name name) *gray-package*)))))

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
(define-stream-generic stream-read-sequence)
(define-stream-generic stream-write-sequence)
(define-stream-generic stream-file-position)

;; Binary streams do not have an element type by default, forcing all child
;; classes to implement it. It makes sense to provide a sensible default
;; implementation.
(defmethod stream-element-type ((stream fundamental-binary-stream))
  '(unsigned-byte 8))
