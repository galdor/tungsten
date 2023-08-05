(in-package :libssh)

(defun connect-session (host &key (port 22))
  (declare (type system:host host)
           (type system:port-number port))
  (let (socket %session)
    (core:abort-protect
        (progn
          (setf socket (system:make-tcp-socket host port))
          (setf %session (libssh-funcall ("ssh_new" (() :pointer))))
          (ssh-options-set/string %session :ssh-options-host host)
          (ssh-options-set/int %session :ssh-options-fd socket)
          (ssh-connect %session)
          %session)
      (when %session
        (ssh-free %session))
      (when socket
        (system:close-fd socket)))))

(defun disconnect-session (%session)
  (declare (type ffi:pointer %session))
  (unwind-protect
       (ssh-disconnect %session)
    (ssh-free %session)))

(defmacro with-session ((session &rest args) &body body)
  `(let ((,session (connect-session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (disconnect-session session))))
