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

(defun client-connection-key (connection)
  (declare (type client-connection connection))
  (with-slots (host port tls) connection
    (list host port tls)))

(defun close-client-connection (connection)
  (declare (type client-connection connection))
  (close (client-connection-stream connection)))

(defvar *client* (make-client))

(defun send-request (request &key (client *client*))
  (declare (type client client)
           (type request request))
  (let* ((key (request-connection-key request))
         (connection (client-connection client key)))
    (finalize-request request)
    (write-request request (client-connection-stream connection)))
  ;; TODO read and return the response
  nil)

(defun finalize-request (request)
  (with-slots (body) request
    (let ((target (request-target-uri (request-target request))))
      ;; Target
      (setf (request-target request) (normalize-request-target target))
      ;; Host header field
      (add-new-request-header-field request "Host"
                                    (request-target-host-header-field target))
      ;; Body
      (when (and body (stringp body))
        (setf body (text:encode-string body)))
      ;; Content-Length header field
      (when body
        (add-new-request-header-field request "Content-Length"
                                      (princ-to-string (length body))))))
  request)
