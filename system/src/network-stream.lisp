(in-package :system)

(defclass network-stream (io-stream)
  ((address
    :type socket-address
    :initarg :address
    :reader network-stream-address)))
