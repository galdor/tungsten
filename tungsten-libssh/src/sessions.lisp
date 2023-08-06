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
     (with-slots (host-key) condition
       (format stream "Unknown SSH server host key SHA256:~A."
               (text:encode-base64 host-key))))))

(define-condition host-key-mismatch (server-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (with-slots (host-key) condition
       (format stream "SSH server host key SHA256:~A does not match ~
                       known host key."
               (text:encode-base64 host-key))))))

(define-condition host-key-type-mismatch (server-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (with-slots (host-key) condition
       (format stream "SSH server host key SHA256:~A does not match ~
                  the type of the known host key."
               (text:encode-base64 host-key))))))

(define-condition client-authentication-error (error)
  ())

(define-condition public-key-authentication-not-supported
    (client-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (with-slots (host-key) condition
       (format stream "SSH public key authentication not supported.")))))

(define-condition public-key-authentication-failure
    (client-authentication-error)
  ()
  (:report
   (lambda (condition stream)
     (with-slots (host-key) condition
       (format stream "SSH public key authentication failed.")))))

(defclass session ()
  ((%session
    :type ffi:pointer
    :initarg :%session
    :reader session-%session)))

(defun open-session (host &key (port 22)
                               accept-unknown-host-key
                               username
                               identity-path private-key-passphrase)
  (declare (type system:host host)
           (type system:port-number port)
           (type boolean accept-unknown-host-key)
           (type (or string null) username private-key-passphrase)
           (type (or pathname string null) identity-path))
  ;; Note that the session owns the socket so we must not try to close it
  ;; once it has been assigned to the session.
  (let* ((socket (system:make-tcp-socket host port))
         (%session (core:abort-protect
                       (ssh-new)
                     (system:close-fd socket)))
         (session
           (core:abort-protect
               (progn
                 (when username
                   (ssh-options-set/string %session :ssh-options-user
                                           username))
                 (ssh-options-set/string %session :ssh-options-host host)
                 (ssh-options-set/int %session :ssh-options-fd socket)
                 (ssh-connect %session)
                 (make-instance 'session :%session %session))
             (ssh-free %session))))
    (core:abort-protect
        (progn
          (handler-bind
              ((unknown-host-key
                 (lambda (condition)
                   (declare (ignore condition))
                   (when accept-unknown-host-key
                     (invoke-restart 'accept-host-key)))))
            (authenticate-server session))
          (handler-bind
              ()
            (authenticate-client
             session
             :identity-path identity-path
             :private-key-passphrase private-key-passphrase))
          session)
      (close-session session))))

(defun close-session (session)
  (declare (type session session))
  (with-slots (%session) session
    (unwind-protect
         (ssh-disconnect %session)
      (ssh-free %session))))

(defmacro with-session ((session &rest args) &body body)
  `(let ((,session (open-session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (close-session session))))

(defun session-host-key (session hash)
  (declare (type session session)
           (type (member :md5 :sha1 :sha256) hash))
  (with-slots (%session) session
    (let ((%key (ssh-get-server-publickey %session))
          (hash-type (ecase hash
                       (:md5 :ssh-publickey-hash-md5)
                       (:sha1 :ssh-publickey-hash-sha1)
                       (:sha256 :ssh-publickey-hash-sha256))))
      (unwind-protect
           (ssh-get-publickey-hash %key hash-type)
        (ssh-key-free %key)))))

(defun authenticate-server (session)
  (declare (type session session))
  (with-slots (%session) session
    (let ((host-key (session-host-key session :sha256)))
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
             :report "Accept the host key as valid."
             (ssh-session-update-known-hosts %session))))
        (:ssh-known-hosts-changed
         (error 'host-key-mismatch :host-key host-key))
        (:ssh-known-hosts-other
         (error 'host-key-type-mismatch :host-key host-key))))))

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

(defun authenticate-client (session &key identity-path private-key-passphrase)
  (declare (type session session)
           (type (or pathname string null) identity-path)
           (type (or string null) private-key-passphrase))
  ;; XXX Should we handle SSH-AUTH-PARTIAL differently?
  (with-slots (%session) session
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
          (error 'public-key-authentication-failure)))))))

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
