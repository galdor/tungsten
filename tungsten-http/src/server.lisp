(in-package :http)

(defclass server ()
  ((mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-server")
    :reader server-mutex)
   (closingp
    :type boolean
    :initform nil)
   (host
    :type system:host
    :initarg :host
    :reader server-host)
   (port
    :type system:port-number
    :initarg :port
    :reader server-port)
   (tcp-server
    :type (or system:tcp-server null)
    :initform nil)
   (request-handler
    :type (or symbol function)
    :initarg :request-handler
    :reader server-request-handler)
   (request-processors
    :type list
    :initform nil)
   (requests
    :type list
    :initform nil)
   (requests-condition-variable
    :type system:condition-variable
    :initform (system:make-condition-variable :name "http-server-requests"))))

(defclass connection ()
  ((server
    :type server
    :initarg :server
    :reader connection-server)
   (stream
    :type system:network-stream
    :initarg :stream
    :reader connection-stream)
   (request-reader
    :type request-reader
    :initform (make-request-reader)
    :accessor connection-request-reader)
   (finishing-p
    :type boolean
    :initform nil
    :accessor connection-finishing-p)))

(defun connection-address (connection)
  (declare (type connection connection))
  (system:network-stream-address (connection-stream connection)))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t)
    (let ((address (connection-address connection)))
      (system:format-socket-address address stream))))

(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :type t)
    (with-slots (host port) server
      (system:format-address host port stream))))

(defun start-server (host port request-handler &key (nb-request-processors 1))
  (declare (type system:host host)
           (type system:port-number port)
           (type (or symbol function) request-handler)
           (type (integer 1) nb-request-processors))
  (let ((server (make-instance 'server :host host :port port
                                       :request-handler request-handler)))
    (core:abort-protect
        (progn
          (flet ((handle-connection (stream)
                   (let ((connection
                           (make-instance 'connection
                                          :server server :stream stream)))
                     (funcall 'server-handle-new-connection
                              server connection))))
            (setf (slot-value server 'tcp-server)
                  (system:start-tcp-server host port #'handle-connection)))
          (dotimes (i nb-request-processors)
            (push (system:make-thread "http-request-processor"
                                      (lambda ()
                                        (server-process-requests server)))
                  (slot-value server 'request-processors)))
          server)
      (stop-server server))))

(defun stop-server (server)
  (declare (type server server))
  (with-slots (mutex closingp tcp-server request-processors requests)
      server
    ;; First stop accepting any new connection and stop the TCP server
    (when tcp-server
      (system:stop-tcp-server tcp-server)
      (setf tcp-server nil)
      t)
    ;; Then stop processing requests
    (system:with-mutex (mutex)
      (setf closingp t))
    (mapc 'system:join-thread request-processors)
    (setf request-processors nil)
    ;; Finally close connections associated with pending requests
    (dolist (request-entry requests)
      (destructuring-bind (request . connection) request-entry
        ;; TODO send a nice error about the server shutting down
        (declare (ignore request))
        (ignore-errors (close-connection connection))))))

(defun server-handle-new-connection (server connection)
  (declare (type server server)
           (type connection connection)
           (ignore server))
  (with-slots (stream) connection
    (core:abort-protect
        (progn
          (setf (system:io-stream-external-format stream)
                '(:ascii :eol-style :crlf))
          (setf (system:tcp-stream-non-blocking stream) t)
          (watch-connection connection '(:read)))
      (close-connection connection))))

(defun server-handle-connection-event (server connection events)
  (declare (type server server)
           (type connection connection)
           (type list events))
  (restart-case
      (handler-bind
          (((or end-of-file connection-closed)
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'close-connection)))
           ((or system:read-event-required system:write-event-required)
             (lambda (condition)
               (declare (ignore condition))
               (return-from server-handle-connection-event)))
           (error
             (lambda (condition)
               (unless core:*interactive*
                 (invoke-restart 'send-error (princ-to-string condition))))))
        (when (member :read events)
          (let ((request (server-read-request server connection)))
            (server-handle-request server request connection)))
        (when (member :write events)
          (let* ((stream (connection-stream connection))
                 (buffer (system:io-stream-write-buffer stream)))
            (force-output stream)
            (when (core:buffer-empty-p buffer)
              (if (connection-finishing-p connection)
                (close-connection connection)
                (watch-connection connection '(:read)))))))
    (close-connection ()
      :report "Close the HTTP connection."
      (close-connection connection))
    (send-error (message)
      :report "Send an HTTP error response and close the connection."
      :test (lambda (condition)
              (declare (ignore condition))
              (not (connection-finishing-p connection)))
      :interactive (lambda () (core:prompt-eval "Error message: "))
      (send-response (make-error-response 500 message) connection)
      (setf (connection-finishing-p connection) t)
      (watch-connection connection '(:write)))))

(defun watch-connection (connection events)
  (declare (type connection connection)
           (type list events))
  (with-slots (stream server) connection
    (with-slots (tcp-server) server
      (system:watch-fd (system:tcp-server-io-base tcp-server)
                       (system:io-stream-fd stream)
                       events
                       (lambda (events)
                         (funcall 'server-handle-connection-event
                                  server connection events))))))

(defun close-connection (connection)
  (declare (type connection connection))
  (with-slots (server stream) connection
    (with-slots (tcp-server) server
      (ignore-errors
       (system:unwatch-fd (system:tcp-server-io-base tcp-server)
                          (system:io-stream-fd stream))
       (close stream)))))

(defun server-read-request (server connection)
  (declare (type server server)
           (type connection connection)
           (ignore server))
  (handler-bind
      ((http-parse-error
         (lambda (condition)
           (send-response (make-error-response 400 (princ-to-string condition))
                          connection)
           (invoke-restart 'close-connection))))
    (read-request (connection-request-reader connection)
                  (connection-stream connection))))

(defun server-handle-request (server request connection)
  (declare (type server server)
           (type request request)
           (type connection connection))
  (with-slots (mutex requests requests-condition-variable) server
    (system:with-mutex (mutex)
      (push (cons request connection) requests)
      (when (null (cdr requests))
        (system:signal-condition-variable requests-condition-variable)))))

(defun server-pop-request (server)
  (declare (type server server))
  (with-slots (mutex requests requests-condition-variable) server
    (system:with-mutex (mutex)
      (unless requests
        (system:wait-condition-variable requests-condition-variable mutex
                                        :timeout 1.0))
      (when requests
        (destructuring-bind (request . connection)
            (pop requests)
          (values request connection))))))

(defun server-process-requests (server)
  (declare (type server server))
  (do ((mutex (server-mutex server)))
      ((system:with-mutex (mutex) (slot-value server 'closingp)))
    (multiple-value-bind (request connection)
        (server-pop-request server)
      (when request
        (restart-case
            (server-process-request server request connection)
          (close-connection ()
            :report "Close the HTTP connection."
            (close-connection connection)))))))

(defun server-process-request (server request connection)
  (declare (type server server)
           (type request request)
           (type connection connection)
           (ignore server))
  ;; TODO request handler
  (send-response (make-response 200) connection)
  (cond
    ((request-keep-connection-alive-p request)
     (setf (connection-request-reader connection) (make-request-reader)))
    (t
     (close-connection connection))))

(defun send-response (response connection)
  (declare (type response response)
           (type connection connection))
  (finalize-response response)
  (write-response response (connection-stream connection))
  (watch-connection connection '(:read :write)))
