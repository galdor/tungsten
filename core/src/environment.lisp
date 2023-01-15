(in-package :core)

(defparameter *interactive* t
  "Indicate whether the current image is running with a user able to interact
with it. This variable should be set to NIL when the image is running as a
background program such as a daemon.")
