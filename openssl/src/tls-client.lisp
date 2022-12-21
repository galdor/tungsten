(in-package :openssl)

(defparameter *default-tls-client-ciphers*
  '("ECDHE-ECDSA-CHACHA20-POLY1305"
    "ECDHE-RSA-CHACHA20-POLY1305"
    "ECDHE-ECDSA-AES256-GCM-SHA384"
    "ECDHE-RSA-AES256-GCM-SHA384"
    "ECDHE-ECDSA-AES128-GCM-SHA256"
    "ECDHE-RSA-AES128-GCM-SHA256"))

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
    (let ((address (system:socket-address client)))
      (write-string (system:format-socket-address address) stream))))

(defmethod close :after ((client tls-client) &key abort)
  (declare (ignore abort))
  (with-slots (%context) client
    (when %context
      (ssl-ctx-free %context)
      (setf %context nil))))

(defun make-tls-client (host port &key (ciphers *default-tls-client-ciphers*)
                                       (verify t)
                                       (verification-depth 20)
                                       ca-certificate-path
                                       ca-certificate-directory
                                       certificate-path
                                       private-key-path)
  "Create and return a TLS client connected to HOST and PORT."
  (declare (type system:host host)
           (type system:port-number port)
           (type list ciphers)
           (type boolean verify)
           (type (integer 0) verification-depth)
           (type (or pathname string null)
                 ca-certificate-path ca-certificate-directory
                 certificate-path private-key-path)
           (ignore ciphers verify verification-depth
                   ca-certificate-path ca-certificate-directory
                   certificate-path private-key-path))
  (let ((%context nil)
        (%ssl nil)
        (success nil))
    (multiple-value-bind (socket address)
        (system::tcp-connect host port)
      (unwind-protect
           (progn
             (setf %context (ssl-ctx-new (tls-client-method)))
             ;; TODO disable <TLS1.2 by default
             ;; TODO SSL_CTX_set_options
             ;; TODO SSL_CTX_set_mode
             ;; TODO SSL_CTX_set_cipher_list
             ;; TODO SSL_CTX_set_verify
             ;; TODO SSL_CTX_set_verify_depth
             ;; TODO SSL_CTX_set_client_CA_list
             ;; TODO SSL_CTX_load_verify_locations
             ;; TODO SSL_CTX_use_certificate_file
             ;; TODO SSL_CTX_use_PrivateKey_file
             ;; TODO SSL_CTX_set_tmp_dh
             (setf %ssl (ssl-new))
             (ssl-set-fd %ssl socket)
             (ssl-connect %ssl)
             (prog1
                 (make-instance 'tls-client :file-descriptor socket
                                            :address address
                                            :host host :port port
                                            :%context %context :%ssl %ssl)
               (setf success t)))
        (unless success
          (when %ssl
            (ssl-free %ssl))
          (when %context
            (ssl-ctx-free %context))
          (system::close-fd socket))))))
