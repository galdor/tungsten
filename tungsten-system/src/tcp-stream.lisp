(in-package :system)

(define-condition read-event-required ()
  ())

(define-condition write-event-required ()
  ())

(define-condition tcp-timeout (error)
  ((stream
    :type network-stream
    :initarg :stream
    :reader tcp-timeout-stream)))

(define-condition tcp-read-timeout (tcp-timeout)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Timeout while reading stream ~S."
             (tcp-timeout-stream condition)))))

(define-condition tcp-write-timeout (tcp-timeout)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Timeout while writing stream ~S."
             (tcp-timeout-stream condition)))))

(defclass tcp-stream (network-stream)
  ((non-blocking
    :type boolean
    :initarg :non-blocking
    :initform nil
    :reader tcp-stream-non-blocking)))

(defmethod initialize-instance :after ((stream tcp-stream)
                                       &key &allow-other-keys)
  (with-slots (fd) stream
    (ffi:with-foreign-value (%one :int)
      (setf (ffi:foreign-value %one :int) 1)
      (setsockopt fd :sol-tcp :tcp-nodelay
                  %one (ffi:foreign-type-size :int)))))

(defmethod close :before ((stream tcp-stream) &key abort)
  (declare (ignore abort))
  (with-slots (fd) stream
    (ignore-errors
     (shutdown fd :shut-rdwr))))

(defun (setf tcp-stream-non-blocking) (value stream)
  (declare (type boolean value)
           (type tcp-stream stream))
  (with-slots (fd non-blocking) stream
    (cond
      ((and non-blocking (null value))
       (fcntl-setfl-add-remove-flags fd nil '(:o-nonblock))
       (setf non-blocking nil))
      ((and (not non-blocking) value)
       (fcntl-setfl-add-remove-flags fd '(:o-nonblock) nil)
       (setf non-blocking t)))))

(defmethod read-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (fd read-timeout non-blocking) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (read-fd fd %data (- end start))
        (system-error (condition)
          (let ((errno (system-error-value condition)))
            (cond
              ((eql errno :econnreset)
               (error 'end-of-file :stream stream))
              ((and non-blocking
                    (or (eql errno :eagain)
                        (eql errno :ewouldblock)))
               (error 'read-event-required))
              ((and (eql errno :eagain) read-timeout)
               (error 'tcp-read-timeout :stream stream))
              (t
               (error condition)))))))))

(defmethod write-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (fd write-buffer write-timeout non-blocking) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (write-fd fd %data (- end start))
        (system-error (condition)
          (let ((errno (system-error-value condition)))
            (cond
              ((or (eql errno :econnreset)
                   (eql errno :epipe))
               (error 'end-of-file :stream stream))
              ((and non-blocking
                    (or (eql errno :eagain) (eql errno :ewouldblock)))
               (error 'write-event-required))
              ((and (eql errno :eagain) write-timeout)
               (error 'tcp-write-timeout :stream stream))
              (t
               (error condition)))))))))
