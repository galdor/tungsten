(in-package :system)

(defclass network-stream (io-stream)
  ((address
    :type socket-address
    :initarg :address
    :reader network-stream-address)
   (read-timeout
    :type (or (real 0) null)
    :initarg :read-timeout
    :initform nil
    :reader network-stream-read-timeout)
   (write-timeout
    :type (or (real 0) null)
    :initarg :write-timeout
    :initform nil
    :reader network-stream-write-timeout)))

(defmethod initialize-instance :after ((stream network-stream)
                                       &key &allow-other-keys)
  (with-slots (fd read-timeout write-timeout) stream
    (when read-timeout
      (setf (network-stream-read-timeout stream) read-timeout))
    (when write-timeout
      (setf (network-stream-write-timeout stream) write-timeout))))

(defun (setf network-stream-read-timeout) (timeout stream)
  (declare (type network-stream stream)
           (type (or real null) timeout))
  (when (null timeout)
    (setf timeout 0))
  (with-slots (fd read-timeout) stream
    (ffi:with-foreign-value (%timeval 'timeval)
      (initialize-timeval %timeval (* (ceiling timeout) 1000000))
      (setsockopt fd :sol-socket :so-rcvtimeo
                  %timeval (ffi:foreign-type-size 'timeval))
      (setf read-timeout timeout))))

(defun (setf network-stream-write-timeout) (timeout stream)
  (declare (type network-stream stream)
           (type (or real null) timeout))
  (when (null timeout)
    (setf timeout 0))
  (with-slots (fd write-timeout) stream
    (ffi:with-foreign-value (%timeval 'timeval)
      (initialize-timeval %timeval (* (ceiling timeout) 1000000))
      (setsockopt fd :sol-socket :so-sndtimeo
                  %timeval (ffi:foreign-type-size 'timeval))
      (setf write-timeout timeout))))
