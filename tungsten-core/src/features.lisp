(in-package :core)

(define-condition unsupported-feature (error)
  ((name
    :type string
    :initarg :name
    :reader unsupported-feature-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "~@(~A~) is not available on the current platform."
               name)))))

(defun unsupported-feature (name)
  (error 'unsupported-feature :name name))

(deftype endianness ()
  '(member :little-endian :big-endian))

(defparameter *endianness*
  #+sbcl
  (cond
    ((member :little-endian *features*)
     :little-endian)
    ((member :big-endian *features*)
     :big-endian)
    (t
     (error "Missing either :LITTLE-ENDIAN or :BIG-ENDIAN in *FEATURES*.")))
  #+ccl
  (cond
    ((member :little-endian-target *features*)
     :little-endian)
    ((member :big-endian-target *features*)
     :big-endian)
    (t
     (error "Missing either :LITTLE-ENDIAN-TARGET or :BIG-ENDIAN-TARGET ~
             in *FEATURES*.")))
  #-(or sbcl ccl)
  (unsupported-feature "endianness detection"))

(defparameter *interactive* t
  "Indicate whether the current image is running with a user able to interact
with it. This variable should be set to NIL when the image is running as a
background program such as a daemon.")
