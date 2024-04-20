(in-package :smtp)

(defclass client ()
  ((host
    :type system:host
    :initarg :host
    :reader client-host)
   (port
    :type system:port-number
    :initarg :port
    :reader client-port)
   (tls
    :type boolean
    :initarg :tls
    :reader client-tls-p)
   (stream
    :type (or system:network-stream null)
    :initarg :stream
    :reader client-stream)
   (server-domains
    :type list
    :initform nil
    :reader client-server-domains)
   (keywords
    :type list
    :initform nil
    :reader client-keywords)
   (credentials
    :initarg :credentials
    :initform nil
    :reader client-credentials)))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (let* ((client-stream (client-stream client))
           (address (system:network-stream-address client-stream)))
      (system:format-socket-address address stream)
      (when (client-tls-p client)
        (write-string " TLS" stream)))))

(defun make-client (host port &key tls (local-host "localhost") credentials)
  (declare (type system:host host)
           (type system:port-number port)
           (type boolean tls))
  (let* ((credentials (finalize-client-credentials host port credentials))
         (external-format '(:ascii :eol-style :crlf))
         (stream
           (cond
             (tls
              (openssl:make-tls-client host port
                                       :external-format external-format))
             (t
              (system:make-tcp-client host port
                                      :external-format external-format)))))
    (core:abort-protect
        (let ((client (make-instance 'client :host host :port port :tls tls
                                             :stream stream
                                             :credentials credentials)))
          (read-greeting-response client)
          (send-ehlo-command client local-host)
          (when credentials
            (authenticate-client client))
          client)
      (close stream))))

(defun disconnect-client (client)
  (declare (type client client))
  (with-slots (stream) client
    (close stream)
    (setf stream nil))
  nil)

(defun client-maximum-message-size (client)
  (declare (type client client))
  (with-slots (keywords) client
    (let ((entry (assoc :size keywords)))
      (when (and entry (> (second entry) 0))
        (second entry)))))

(defun finalize-client-credentials (host port credentials)
  (declare (type system:host host)
           (type system:port-number port)
           (type list credentials))
  (let* ((username (cdr (assoc :username credentials)))
         (password (lookup-netrc-password host port username)))
    (unless (assoc :password credentials)
      (when password
        (cons (cons :password password) credentials)))))

(defun lookup-netrc-password (host port &optional username)
  (let ((entry (car (netrc:search-entries :machine host
                                          :port port
                                          :login username))))
    (when entry
      (netrc:entry-password entry))))

(defun client-credential (client name)
  (declare (type client client)
           (type symbol name))
  (cdr (assoc name (client-credentials client))))

(defun send-message (client message &key reverse-path)
  (declare (type client client)
           (type imf:message message)
           (type (or imf:mailbox string null) reverse-path))
  (let ((reverse-path
          (or reverse-path
              (imf:message-field message "From")
              (error "cannot identify reverse path without a \"From\" ~
                      header field")))
        (recipient
          (or (imf:message-field message "To")
              (error "missing \"To\" header field")))
        (mail-parameters nil))
    (send-mail-command client reverse-path mail-parameters)
    (send-rcpt-command client recipient)
    (send-data-command client)
    (imf:write-message message (client-stream client)
                       :max-line-length 78 :smtp t)
    (send-message-end client)))
