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

(defclass session ()
  ((%session
    :type ffi:pointer
    :initarg :%session
    :reader session-%session)))

(defun open-session (host &key (port 22)
                               accept-unknown-host-key)
  (declare (type system:host host)
           (type system:port-number port))
  ;; Note that the session owns the socket so we must not try to close it
  ;; once it has been assigned to the session.
  (let* ((socket (system:make-tcp-socket host port))
         (%session (core:abort-protect
                       (ssh-new)
                     (system:close-fd socket)))
         (session
           (core:abort-protect
               (progn
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
             nil)))
        (:ssh-known-hosts-changed
         (error 'host-key-mismatch :host-key host-key))
        (:ssh-known-hosts-other
         (error 'host-key-type-mismatch :host-key host-key))))))
