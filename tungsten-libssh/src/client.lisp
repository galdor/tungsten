(in-package :libssh)

(defparameter *client-read-block-size* 1024)

(defclass client ()
  ((%session
    :type (or ffi:pointer null)
    :initarg :%session)))

(defun make-client (host &key (port 22)
                              accept-unknown-host-key
                              username
                              identity-path private-key-passphrase)
  (declare (type system:host host)
           (type system:port-number port)
           (type boolean accept-unknown-host-key)
           (type (or string null) username private-key-passphrase)
           (type (or pathname string null) identity-path))
  (let (socket %session session-owns-socket session-connected)
    (core:abort-protect
        (progn
          (setf socket (system:make-tcp-socket host port))
          (setf %session (ssh-new))
          (when username
            (ssh-options-set/string %session :ssh-options-user username))
          (ssh-options-set/string %session :ssh-options-host host)
          (ssh-options-set/int %session :ssh-options-fd socket)
          (setf session-owns-socket t)
          (ssh-connect %session)
          (setf session-connected t)
          (handler-bind
              ((unknown-host-key
                 (lambda (condition)
                   (declare (ignore condition))
                   (when accept-unknown-host-key
                     (invoke-restart 'accept-host-key)))))
            (authenticate-server %session))
          (authenticate-client %session
                               :identity-path identity-path
                               :private-key-passphrase private-key-passphrase)
          (make-instance 'client :%session %session))
      (when %session
        (when session-connected
          (ignore-errors (ssh-disconnect %session)))
        (ssh-free %session))
      (when (and socket (not session-owns-socket))
        (system:close-fd socket)))))

(defun disconnect-client (client)
  (declare (type client client))
  (with-slots (%session) client
    (ignore-errors (ssh-disconnect %session))
    (ssh-free %session)
    (setf %session nil)))

(defmacro with-client ((client &rest arguments) &body body)
  `(let ((,client (make-client ,@arguments)))
     (unwind-protect
          (progn
            ,@body)
       (disconnect-client ,client))))

(defmacro with-channel ((%channel %session) &body body)
  (let ((%session-var (gensym "%SESSION-VAR")))
    `(let* ((,%session-var ,%session)
            (,%channel (ssh-channel-new ,%session-var)))
       (unwind-protect
            (progn
              (ssh-channel-open-session %channel %session)
              ,@body)
         (ignore-errors (ssh-channel-close ,%channel ,%session-var))
         (ssh-channel-free ,%channel)))))

(defun execute (client command)
  (declare (type client client)
           (type string command))
  (with-slots (%session) client
    (with-channel (%channel %session)
      (ssh-channel-request-exec %channel %session command)
      (flet ((read-output (buffer stderr)
               (declare (type core:buffer buffer)
                        (type boolean stderr))
               (let ((offset
                       (core:buffer-reserve buffer *client-read-block-size*)))
                 (ffi:with-pinned-vector-data
                     (%buffer (core:buffer-data buffer) offset)
                   (let ((nb-read (ssh-channel-read %channel %session %buffer
                                                    *client-read-block-size*
                                                    stderr)))
                     (incf (core:buffer-end buffer) nb-read)
                     nb-read)))))
        (do ((stdout (core:make-buffer *client-read-block-size*))
             (stdout-closed-p nil)
             (stderr (core:make-buffer *client-read-block-size*))
             (stderr-closed-p nil))
            ((and stdout-closed-p stderr-closed-p)
             (ssh-channel-send-eof %channel %session)
             (values (ssh-channel-get-exit-status %channel %session)
                     (text:decode-string (core:buffer-content stdout))
                     (text:decode-string (core:buffer-content stderr))))
          (unless stdout-closed-p
            (setf stdout-closed-p (zerop (read-output stdout nil))))
          (unless stderr-closed-p
            (setf stderr-closed-p (zerop (read-output stderr t)))))))))
