(in-package :streams)

;; Binary streams do not have an element type by default, forcing all
;; subclasses to implement it. It makes sense to provide a sensible default
;; implementation.
(defmethod stream-element-type ((stream fundamental-binary-stream))
  '(unsigned-byte 8))
