(in-package :http)

(deftype connection-key ()
  'list)

(defclass client ()
  ((connections
    :type hash-table
    :initform (make-hash-table :test #'equal))))

(defclass client-connection ()
  ((host
    :type system:host
    :initarg :host
    :reader client-connection-host)
   (port
    :type system:port-number
    :initarg :port
    :reader client-connection-port)
   (tls
    :type boolean
    :initarg :tls
    :reader client-connection-tls-p)
   (stream
    :type system:network-stream
    :initarg :stream
    :reader client-connection-stream)))

(defmethod print-object ((connection client-connection) stream)
  (print-unreadable-object (connection stream :type t)
    (let* ((connection-stream (client-connection-stream connection))
           (address (system:network-stream-address connection-stream)))
      (system:format-socket-address address stream)
      (when (client-connection-tls-p connection)
        (write-string " TLS" stream)))))

(defun make-client ()
  (make-instance 'client))

(defun client-connections (client)
  (declare (type client client))
  (let ((connection-list nil))
    (with-slots (connections) client
      (maphash (lambda (key connection)
                 (declare (ignore key))
                 (push connection connection-list))
               connections))
    connection-list))

(defun client-connection (client key)
  (declare (type client client)
           (type connection-key key))
  (with-slots (connections) client
    (or (gethash key connections)
        (destructuring-bind (host port tls) key
          (connect-client client host port tls)))))

(defun connect-client (client host port tls)
  (declare (type client client)
           (type system:host host)
           (type system:port-number port)
           (type boolean tls))
  (let* ((external-format '(:ascii :eol-style :crlf))
         (stream
           (cond
             (tls
              (openssl:make-tls-client host port
                                       :external-format external-format))
             (t
              (system:make-tcp-client host port
                                      :external-format external-format))))
         (connection
           (make-instance 'client-connection :host host :port port :tls tls
                                             :stream stream))
         (key (client-connection-key connection)))
    (with-slots (connections) client
      (let ((previous-connection (gethash key connections)))
        (when previous-connection
          (close-client-connection previous-connection)))
      (setf (gethash key connections) connection))
    connection))

(defun disconnect-client (client)
  (declare (type client client))
  (with-slots (connections) client
    (maphash (lambda (key connection)
               (declare (ignore key))
               (close-client-connection connection))
             connections)
    (setf connections (make-hash-table :test #'equal)))
  nil)

(defun discard-client-connection (client connection)
  (declare (type client client)
           (type client-connection connection))
  (close-client-connection connection)
  (with-slots (connections) client
    (remhash (client-connection-key connection) connections)))

(defun client-connection-key (connection)
  (declare (type client-connection connection))
  (with-slots (host port tls) connection
    (list host port tls)))

(defun close-client-connection (connection)
  (declare (type client-connection connection))
  (ignore-errors (close (client-connection-stream connection))))

(defvar *client* (make-client))

(defun send-request (method uri &key header body
                                     (client *client*)
                                     (follow-redirections t)
                                     (max-redirections 10))
  (declare (type client client)
           (type request-method method)
           (type (or uri:uri string) uri)
           (type header header)
           (type (or body null) body))
  (let* ((uri (when (stringp uri) (uri:parse uri)))
         (body (when (and body (stringp body))
                 (text:encode-string body)))
         (request (make-instance 'request :method method
                                          :target uri
                                          :version :http-1.1
                                          :header header
                                          :body body)))
    (when body
      (add-new-request-header-field request "Content-Length"
                                    (princ-to-string (length body))))
    (let ((response (send-request* request :client client)))
      (cond
        (follow-redirections
         (do ((nb-redirections 0))
             ((>= nb-redirections max-redirections)
              (error 'too-many-redirections))
           (let ((location (response-redirection-location response)))
             (unless location
               (return response))
             (redirect-request request response location uri)
             (setf response (send-request* request :client client))
             (incf nb-redirections))))
        (t
         response)))))

(defun send-request* (request &key (client *client*))
  (let* ((target-uri (request-target request))
         (key (uri-connection-key target-uri))
         (connection (client-connection client key))
         (stream (client-connection-stream connection)))
    (setf (request-target request) (make-request-target target-uri))
    (setf (request-header-field request "Host") (host-header-field target-uri))
    (core:abort-protect
        (handler-case
            (progn
              (write-request request stream)
              (read-response stream))
          (end-of-file ()
            (error 'connection-closed)))
      (discard-client-connection client connection))))

(defun uri-connection-key (target)
  (declare (type uri:uri target))
  (let* ((scheme (or (uri:uri-scheme target) "http"))
         (host (restart-case
                   (let ((host (uri:uri-host target)))
                     (or host
                         (error 'missing-request-target-host :target target)))
                 (set-host (host)
                   :report "Set the host to connect to."
                   :interactive (lambda () (core:prompt-eval "Host string: "))
                   host)))
         (port (let ((port (uri:uri-port target)))
                 (cond
                   (port port)
                   ((equalp scheme "http") 80)
                   ((equalp scheme "https") 443))))
         (tls (equalp scheme "https")))
    (list host port tls)))
