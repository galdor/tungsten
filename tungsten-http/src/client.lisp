(in-package :http)

(defparameter *client-netrc-authorization-scheme* :basic
  "The HTTP authorization scheme used when applying netrc entries to requests.
Must be either NIL, :BASIC or :BEARER.")

(deftype connection-key ()
  'list)

(defclass client ()
  ((connections
    :type hash-table
    :initform (make-hash-table :test #'equal))
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-client"))))

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
    (with-slots (connections mutex) client
      (system:with-mutex (mutex)
        (maphash (lambda (key connection)
                   (declare (ignore key))
                   (push connection connection-list))
                 connections)))
    connection-list))

(defun client-connection (client key)
  (declare (type client client)
           (type connection-key key))
  (with-slots (connections mutex) client
    ;; Careful, CONNECT-CLIENT uses the mutex
    (or (system:with-mutex (mutex)
          (gethash key connections))
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
    (with-slots (connections mutex) client
      (system:with-mutex (mutex)
        (let ((previous-connection (gethash key connections)))
          (when previous-connection
            (close-client-connection previous-connection)))
        (setf (gethash key connections) connection)))
    connection))

(defun disconnect-client (client)
  (declare (type client client))
  (with-slots (connections mutex) client
    (system:with-mutex (mutex)
      (maphash (lambda (key connection)
                 (declare (ignore key))
                 (close-client-connection connection))
               connections)
      (setf connections (make-hash-table :test #'equal))))
  nil)

(defun discard-client-connection (client connection)
  (declare (type client client)
           (type client-connection connection))
  (close-client-connection connection)
  (with-slots (connections mutex) client
    (system:with-mutex (mutex)
      (remhash (client-connection-key connection) connections))))

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
  (let* ((uri (uri:uri uri))
         (body (when (and body (stringp body))
                 (text:encode-string body)))
         (request (make-instance 'request :method method
                                          :target uri
                                          :version :http-1.1
                                          :header header
                                          :body body)))
    (when (or body
              (eq method :post)
              (equalp method "POST")
              (eq method :put)
              (equalp method "PUT"))
      (let ((length (if body (length body) 0)))
        (add-new-request-header-field request "Content-Length"
                                      (princ-to-string length))))
    (let ((response (finalize-and-send-request request client)))
      (cond
        (follow-redirections
         (do ((nb-redirections 0))
             ((>= nb-redirections max-redirections)
              (error 'too-many-redirections))
           (let ((location (response-redirection-location response)))
             (unless location
               (return response))
             (redirect-request request response location uri)
             (setf response (finalize-and-send-request request client))
             (incf nb-redirections))))
        (t
         response)))))

(defun finalize-and-send-request (request client)
  (declare (type request request)
           (type client client))
  ;; Use a netrc entry if there is one matching the target host. Note that we
  ;; do this before extracting the key since applying the netrc entry can
  ;; change the port of the target URI.
  (let* ((target (request-target request))
         (host (uri:uri-host target))
         (entries (when *client-netrc-authorization-scheme*
                    (netrc:search-entries :machine host))))
    (when (and *client-netrc-authorization-scheme* entries)
      (apply-netrc-entry (car entries) request))
    (let ((key (uri-connection-key target)))
      (add-new-request-header-field request "User-Agent" (user-agent))
      (setf (request-target request) (make-request-target target))
      (setf (request-header-field request "Host") (host-header-field target))
      ;; We cannot detect if a connection in the pool has timed out or failed
      ;; due to a random IO error. When that happens, we will be notified
      ;; while reading the response (read() will return zero). It makes sense
      ;; to automatically retry at least once in this case.
      (do ((nb-attempts 1 (1+ nb-attempts)))
          (nil)
        (flet ((maybe-reconnect ()
                 (lambda (condition)
                   (declare (ignore condition))
                   (when (= nb-attempts 1)
                     (invoke-restart 'reconnect)))))
          (restart-case
              (handler-bind
                  ((connection-closed (maybe-reconnect))
                   (system:system-error (maybe-reconnect))
                   (openssl:openssl-error-stack (maybe-reconnect)))
                (return-from finalize-and-send-request
                  (send-finalized-request request client key)))
            (reconnect ()
              :report "Reconnect and try to send the request again."
              nil)))))))

(defun apply-netrc-entry (entry request)
  (declare (type netrc:entry entry)
           (type request request))
  (let ((target (request-target request))
        (entry-port (netrc:entry-port entry))
        (entry-login (netrc:entry-login entry))
        (entry-password (netrc:entry-password entry)))
    (when (and entry-port (null (uri:uri-port target)))
      (setf (uri:uri-port target) entry-port))
    (when entry-login
      (let ((value
              (ecase *client-netrc-authorization-scheme*
                (:basic
                 (basic-authorization-header-field-value
                  entry-login entry-password))
                (:bearer
                 (bearer-authorization-header-field-value entry-password)))))
        (add-new-request-header-field request "Authorization" value))))
  request)

(defun send-finalized-request (request client connection-key)
  (declare (type request request)
           (type connection-key connection-key)
           (type client client))
  (let ((connection (client-connection client connection-key)))
    (core:abort-protect
        (handler-case
            (let ((stream (client-connection-stream connection)))
              (write-request request stream)
              (read-response stream))
          (end-of-file ()
            (error 'connection-closed)))
      (discard-client-connection client connection))))

(defun redirect-request (request response location uri)
  (declare (type request request)
           (type response response)
           (type uri:uri location uri))
  (with-slots (method target header body) request
    (setf target (uri:resolve-reference location uri))
    (when (eql (http:response-status response) 303)
      (setf header (delete-header-fields '("Content-Encoding"
                                           "Content-Language"
                                           "Content-Location"
                                           "Content-Type"
                                           "Content-Length"
                                           "Digest")
                                         header))
      (setf method :get)
      (setf body nil))))

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

(defun user-agent ()
  "Return the default user agent used in client requests."
  "Tungsten/dev")

(defun basic-authorization-header-field-value (login password)
  (let ((credentials
          (format nil "~A:~@[~A~]" login password)))
    (concatenate 'string "Basic "
                 (text:encode-base64
                  (text:encode-string credentials)))))

(defun bearer-authorization-header-field-value (token)
  (concatenate 'string "Bearer " token))
