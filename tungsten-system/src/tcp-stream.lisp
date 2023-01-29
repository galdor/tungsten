(in-package :system)

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
  ())

(defmethod close :before ((stream tcp-stream) &key abort)
  (declare (ignore abort))
  (with-slots (fd) stream
    (ignore-errors
     (shutdown fd :shut-rdwr))))

(defmethod read-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (fd read-timeout) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (read-fd fd %data (- end start))
        (system-error (condition)
          (let ((errno (system-error-value condition)))
            (cond
              ((eql errno :econnreset)
               (error 'end-of-file :stream stream))
              ((and (eql errno :eagain) read-timeout)
               (error 'tcp-read-timeout :stream stream))
              (t
               (error condition)))))))))

(defmethod write-io-stream ((stream tcp-stream) octets start end)
  (declare (type core:octet-vector octets)
           (type (integer 0) start end))
  (with-slots (fd write-buffer write-timeout) stream
    (ffi:with-pinned-vector-data (%data octets start)
      (handler-case
          (write-fd fd %data (- end start))
        (system-error (condition)
          (let ((errno (system-error-value condition)))
            (cond
              ((or (eql errno :econnreset)
                   (eql errno :epipe))
               (error 'end-of-file :stream stream))
              ((and (eql errno :eagain) write-timeout)
               (error 'tcp-write-timeout :stream stream))
              (t
               (error condition)))))))))
