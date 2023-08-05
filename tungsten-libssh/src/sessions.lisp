(in-package :libssh)

(defclass session ()
  ((%session
    :type ffi:pointer
    :initarg :%session
    :reader session-%session)))

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
          (make-instance 'session :%session %session))
      (when %session
        (ssh-free %session))
      (when socket
        (system:close-fd socket)))))

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
