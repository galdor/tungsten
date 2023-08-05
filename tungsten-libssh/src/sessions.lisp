(in-package :libssh)

(defclass session ()
  ((%session
    :type ffi:pointer
    :initarg :%session
    :reader session-%session)))

(defun connect-session (host &key (port 22))
  (declare (type system:host host)
           (type system:port-number port))
  ;; Note that the session owns the socket so we must not try to close it
  ;; once it has been assigned to the session.
  (let* ((socket (system:make-tcp-socket host port))
         (%session (core:abort-protect
                       (ssh-new)
                     (system:close-fd socket))))
    (core:abort-protect
        (progn
          (ssh-options-set/string %session :ssh-options-host host)
          (ssh-options-set/int %session :ssh-options-fd socket)
          (ssh-connect %session)
          (make-instance 'session :%session %session))
      (ssh-free %session))))

(defun disconnect-session (session)
  (declare (type session session))
  (with-slots (%session) session
    (unwind-protect
         (ssh-disconnect %session)
      (ssh-free %session))))

(defmacro with-session ((session &rest args) &body body)
  `(let ((,session (connect-session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (disconnect-session session))))

(defun session-public-key-hash (session hash)
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
