(in-package :libssh)

(defun connect-session (host &key (port 22))
  (declare (type string host)
           (type system:port-number port))
  (let ((%session (libssh-funcall ("ssh_new" (() :pointer)))))
    (core:abort-protect
        (progn
          (ssh-options-set/string %session :ssh-options-host host)
          (ssh-options-set/unsigned-int %session :ssh-options-port port)
          (libssh-funcall ("ssh_connect" ((:pointer) ssh-error) %session)
                          :error-source %session)
          %session)
      (libssh-funcall ("ssh_free" ((:pointer) :void) %session)))))

(defun disconnect-session (%session)
  (declare (type ffi:pointer %session))
  (unwind-protect
       (libssh-funcall ("ssh_disconnect" ((:pointer) :void) %session))
    (libssh-funcall ("ssh_free" ((:pointer) :void) %session))))

(defmacro with-session ((session &rest args) &body body)
  `(let ((,session (connect-session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (disconnect-session session))))
