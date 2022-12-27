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
                                       (peer-verification t)
                                       (peer-verification-depth 20)
                                       ca-certificate-path
                                       ca-certificate-directory-path
                                       certificate-path
                                       private-key-path)
  "Create and return a TLS client connected to HOST and PORT."
  (declare (type system:host host)
           (type system:port-number port)
           (type list ciphers)
           (type boolean peer-verification)
           (type (integer 0) peer-verification-depth)
           (type (or pathname string null)
                 ca-certificate-path ca-certificate-directory-path
                 certificate-path private-key-path))
  (let ((%context nil)
        (%ssl nil)
        (success nil))
    (multiple-value-bind (socket address)
        (system::tcp-connect host port)
      (unwind-protect
           (progn
             (setf %context (ssl-ctx-new (tls-client-method)))
             (ssl-ctx-set-options %context '(:ssl-op-all))
             (ssl-ctx-set-min-proto-version %context :tls1-2-version)
             (ssl-ctx-set-cipher-list %context ciphers)
             (when peer-verification
               (ssl-ctx-set-verify %context '(:ssl-verify-peer) nil))
             (when peer-verification-depth
               (ssl-ctx-set-verify-depth %context peer-verification-depth))
             (when ca-certificate-path
               (ssl-ctx-load-verify-file %context ca-certificate-path))
             (when ca-certificate-directory-path
               (ssl-ctx-load-verify-dir %context
                                        ca-certificate-directory-path))
             (when certificate-path
               (ssl-ctx-use-certificate-file %context certificate-path
                                             :ssl-filetype-pem))
             (when private-key-path
               (ssl-ctx-use-private-key-file %context private-key-path
                                             :ssl-filetype-pem))
             (setf %ssl (ssl-new %context))
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
