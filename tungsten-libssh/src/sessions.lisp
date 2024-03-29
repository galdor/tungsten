(in-package :libssh)

(define-condition server-authentication-error (error)
  ((host-key
    :type core:octet-vector
    :initarg :host-key
    :reader server-authentication-error-host-key)))

(define-condition unknown-host-key (server-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "unknown SSH server host key SHA256:~A"
             (text:encode-base64
              (server-authentication-error-host-key condition))))))

(define-condition host-key-mismatch (server-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "SSH server host key SHA256:~A does not match ~
                     known host key"
             (text:encode-base64
              (server-authentication-error-host-key condition))))))

(define-condition host-key-type-mismatch (server-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "SSH server host key SHA256:~A does not match ~
                     the type of the known host key"
             (text:encode-base64
              (server-authentication-error-host-key condition))))))

(define-condition client-authentication-error (error)
  ())

(define-condition public-key-authentication-not-supported
    (client-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "SSH public key authentication not supported"))))

(define-condition public-key-authentication-failure
    (client-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "SSH public key authentication failed"))))

(defun session-host-key (%session hash)
  (declare (type ffi:pointer %session)
           (type (member :md5 :sha1 :sha256) hash))
  (let ((%key (ssh-get-server-publickey %session))
        (hash-type (ecase hash
                     (:md5 :ssh-publickey-hash-md5)
                     (:sha1 :ssh-publickey-hash-sha1)
                     (:sha256 :ssh-publickey-hash-sha256))))
    (unwind-protect
         (ssh-get-publickey-hash %key hash-type)
      (ssh-key-free %key))))

(defun authenticate-server (%session)
  (declare (type ffi:pointer %session))
  (let ((host-key (session-host-key %session :sha256)))
    (ecase (ssh-session-is-known-server %session)
      (:ssh-known-hosts-ok
       nil)
      ((:ssh-known-hosts-not-found
        :ssh-known-hosts-unknown)
       ;; SSH_KNOWN_HOSTS_NOT_FOUND means that the host key is unknown, the
       ;; same way as SSH_KNOWN_HOSTS_UNKNOWN, but that in addition there is
       ;; no known_host file found. We only care about the host key though.
       (restart-case
           (error 'unknown-host-key :host-key host-key)
         (accept-host-key ()
           :report "accept the host key as valid"
           (ssh-session-update-known-hosts %session))))
      (:ssh-known-hosts-changed
       (error 'host-key-mismatch :host-key host-key))
      (:ssh-known-hosts-other
       (error 'host-key-type-mismatch :host-key host-key)))))

(defmacro with-identity-keys ((%public-key %private-key identity-path
                               &key private-key-passphrase)
                              &body body)
  `(multiple-value-bind (,%public-key ,%private-key)
       (load-identity ,identity-path
                      :private-key-passphrase ,private-key-passphrase)
     (unwind-protect
          (progn
            ,@body)
       (ssh-key-free ,%public-key)
       (ssh-key-free ,%private-key))))

(defun authenticate-client (%session &key identity-path private-key-passphrase)
  (declare (type ffi:pointer %session)
           (type (or pathname string null) identity-path)
           (type (or string null) private-key-passphrase))
  ;; XXX Should we handle SSH-AUTH-PARTIAL differently?
  ;;
  ;; ssh_userauth_none() must always be called before calling
  ;; ssh_userauth_list().
  (when (eq (ignore-errors (ssh-userauth-none %session))
            :ssh-auth-sucess)
    (return-from authenticate-client t))
  (unless (member :ssh-auth-method-publickey
                  (ssh-userauth-list %session))
    (error 'public-key-authentication-not-supported))
  (cond
    ;; If we are using a specific identity file, load and use it
    (identity-path
     (with-identity-keys
         (%public-key %private-key identity-path
                      :private-key-passphrase private-key-passphrase)
       (ecase (ssh-userauth-try-publickey %session %public-key)
         (:ssh-auth-success
          nil)
         ((:ssh-auth-denied :ssh-auth-partial)
          (error 'public-key-authentication-failure)))
       (ecase (ssh-userauth-try-publickey %session %private-key)
         (:ssh-auth-success
          nil)
         ((:ssh-auth-denied :ssh-auth-partial)
          (error 'public-key-authentication-failure)))))
    (t
     ;; If not, fallback to the automatic function which sends known public
     ;; keys to the server until one matches then load and use the
     ;; associated private key.
     (ecase (ssh-userauth-publickey-auto %session
                                         private-key-passphrase)
       (:ssh-auth-success
        nil)
       ((:ssh-auth-denied :ssh-auth-partial)
        (error 'public-key-authentication-failure))))))

(defun load-identity (path &key private-key-passphrase)
  (declare (type (or pathname string) path)
           (type (or string null) private-key-passphrase))
  (let (%public-key %private-key)
    (core:abort-protect
        (let ((private-key-data
                (system:read-file path :external-format :ascii)))
          (setf %private-key
                (ssh-pki-import-privkey-base64 private-key-data
                                               private-key-passphrase))
          (setf %public-key (ssh-pki-export-privkey-to-pubkey %private-key))
          (values %public-key %private-key))
      (when %public-key
        (ssh-key-free %public-key))
      (when %private-key
        (ssh-key-free %private-key)))))
