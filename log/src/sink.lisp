(in-package :log)

(defclass sink ()
  ())

(defgeneric write-message (message sink))

(defclass default-sink (sink)
  ())

(defun make-default-sink ()
  (make-instance 'default-sink))

(defmethod write-message (message (sink default-sink))
  (declare (type message message))
  (with-slots (domain level text) message
    (let ((*print-case* :downcase))
      (format *standard-output* "~24@<~{~A~^.~}~>  ~5@<~A~>  ~A~%"
              domain level text))))
