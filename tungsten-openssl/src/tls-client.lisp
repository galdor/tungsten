(in-package :openssl)

(defparameter *default-tls-client-ciphers*
  '("ECDHE-ECDSA-CHACHA20-POLY1305"
    "ECDHE-RSA-CHACHA20-POLY1305"
    "ECDHE-ECDSA-AES256-GCM-SHA384"
    "ECDHE-RSA-AES256-GCM-SHA384"
    "ECDHE-ECDSA-AES128-GCM-SHA256"
    "ECDHE-RSA-AES128-GCM-SHA256"))

(defparameter *default-peer-verification-depth* 20)

(defparameter *default-ca-certificate-directory-paths*
  (let ((paths nil))
    (flet ((try (path)
             (when (probe-file path)
               (push path paths))))
      (try #p"/etc/ssl/certs/"))
    paths))

(defclass tls-client (tls-stream)
  ((host
    :type system:host
    :initarg :host
    :reader tls-client-host)
   (port
    :type system:port-number
    :initarg :port
    :reader tls-client-port)
   (%context
    :type (or ffi:pointer null)
    :initarg :%context)))

(defmethod print-object ((client tls-client) stream)
  (print-unreadable-object (client stream :type t)
    (let ((address (system:network-stream-address client)))
      (write-string (system:format-socket-address address) stream))))

(defmethod close :after ((client tls-client) &key abort)
  (declare (ignore abort))
  (with-slots (%context) client
    (when %context
      (ssl-ctx-free %context)
      (setf %context nil))))

(defun make-tls-client (host port
                        &key (external-format text:*default-external-format*)
                             (read-timeout
                              system:*default-tcp-client-read-timeout*)
                             (write-timeout
                              system:*default-tcp-client-write-timeout*)
                             (ciphers *default-tls-client-ciphers*)
                             (peer-verification t)
                             (peer-verification-depth
                              *default-peer-verification-depth*)
                             ca-certificate-paths
                             (ca-certificate-directory-paths
                              *default-ca-certificate-directory-paths*)
                             certificate-path
                             private-key-path
                             server-name)
  "Create and return a TLS client connected to HOST and PORT."
  (declare (type system:host host)
           (type system:port-number port)
           (type (or (real 0) null) read-timeout write-timeout)
           (type list ciphers)
           (type boolean peer-verification)
           (type (integer 0) peer-verification-depth)
           (type list ca-certificate-paths ca-certificate-directory-paths)
           (type (or pathname string null) certificate-path private-key-path)
           (type (or string null) server-name))
  (let ((tcp-client nil))
    (core:abort-protect
        (progn
          (setf tcp-client (system:make-tcp-client host port))
          (init-tls-client tcp-client
                           :external-format external-format
                           :read-timeout read-timeout
                           :write-timeout write-timeout
                           :ciphers ciphers
                           :peer-verification peer-verification
                           :peer-verification-depth peer-verification-depth
                           :ca-certificate-paths ca-certificate-paths
                           :ca-certificate-directory-paths
                           ca-certificate-directory-paths
                           :certificate-path certificate-path
                           :private-key-path private-key-path
                           :server-name server-name))
      (close tcp-client))))

(defun init-tls-client (tcp-client
                        &key (external-format text:*default-external-format*)
                             (read-timeout
                              system:*default-tcp-client-read-timeout*)
                             (write-timeout
                              system:*default-tcp-client-write-timeout*)
                             (ciphers *default-tls-client-ciphers*)
                             (peer-verification t)
                             (peer-verification-depth
                              *default-peer-verification-depth*)
                             ca-certificate-paths
                             (ca-certificate-directory-paths
                              *default-ca-certificate-directory-paths*)
                             certificate-path
                             private-key-path
                             server-name)
  (declare (type system:tcp-client tcp-client)
           (type (or (real 0) null) read-timeout write-timeout)
           (type list ciphers)
           (type boolean peer-verification)
           (type (integer 0) peer-verification-depth)
           (type list ca-certificate-paths ca-certificate-directory-paths)
           (type (or pathname string null) certificate-path private-key-path)
           (type (or string null) server-name))
  (let ((%context nil)
        (%ssl nil)
        (fd (system:io-stream-fd tcp-client))
        (address (system:network-stream-address tcp-client))
        (host (system:tcp-client-host tcp-client))
        (port (system:tcp-client-port tcp-client)))
    (core:abort-protect
        (progn
          (setf %context (ssl-ctx-new (tls-client-method)))
          (ssl-ctx-set-options %context '(:ssl-op-all))
          (ssl-ctx-set-min-proto-version %context :tls1-2-version)
          (ssl-ctx-set-cipher-list %context ciphers)
          (when peer-verification
            (ssl-ctx-set-verify %context '(:ssl-verify-peer) nil))
          (when peer-verification-depth
            (ssl-ctx-set-verify-depth %context peer-verification-depth))
          (dolist (path ca-certificate-paths)
            (ssl-ctx-load-verify-file %context path))
          (dolist (path ca-certificate-directory-paths)
            (ssl-ctx-load-verify-dir %context path))
          (when certificate-path
            (ssl-ctx-use-certificate-file %context certificate-path
                                          :ssl-filetype-pem))
          (when private-key-path
            (ssl-ctx-use-private-key-file %context private-key-path
                                          :ssl-filetype-pem))
          (setf %ssl (ssl-new %context))
          (cond
            (server-name
             (ssl-set-tlsext-host-name %ssl server-name))
            ((stringp host)
             (ssl-set-tlsext-host-name %ssl host)))
          (ssl-set-fd %ssl fd)
          (ssl-connect %ssl)
          (make-instance 'tls-client :fd fd
                                     :address address
                                     :external-format external-format
                                     :host host :port port
                                     :read-timeout read-timeout
                                     :write-timeout write-timeout
                                     :%context %context :%ssl %ssl))
      (when %ssl
        (ssl-free %ssl))
      (when %context
        (ssl-ctx-free %context)))))
