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
  (with-slots (domain level text-format text-arguments) message
    (let ((*print-case* :downcase))
      (format *standard-output* "~5@<~A~>  ~24@<~{~A~^.~}~>  ~?~%"
              level domain text-format text-arguments))))
