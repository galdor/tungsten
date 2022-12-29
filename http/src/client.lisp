(in-package :http)

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
   (socket
    :type system:socket
    :initarg :socket
    :reader client-connection-socket)))

(defmethod print-object ((connection client-connection) stream)
  (print-unreadable-object (connection stream :type t)
    (let* ((socket (client-connection-socket connection))
           (address (system:socket-address socket)))
      (write-string (system:format-socket-address address) stream)
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

(defun connect-client (client host port tls)
  (declare (type client client)
           (type system:host host)
           (type system:port-number port)
           (type boolean tls))
  (let* ((socket
           (cond
             (tls
              (openssl:make-tls-client host port))
             (t
              (system:make-tcp-client host port))))
         (connection
           (make-instance 'client-connection :host host :port port :tls tls
                                             :socket socket))
         (key (client-connection-key connection)))
    (with-slots (connections) client
      (let ((previous-connection (gethash key connections)))
        (when previous-connection
          (close-client-connection previous-connection)))
      (setf (gethash key connections) connection))))

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
  (close (client-connection-socket connection)))