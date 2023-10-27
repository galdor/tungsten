(in-package :log)

(defclass sink ()
  ())

(defgeneric write-message (message sink))
