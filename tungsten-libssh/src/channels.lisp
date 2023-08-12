(in-package :libssh)

(defclass channel ()
  ((%channel
    :type ffi:pointer
    :initarg :%channel
    :reader channel-%channel)
   (%session
    :type ffi:pointer
    :initarg :%session
    :reader channel-%session)))

(defun open-channel (session)
  (declare (type session session))
  (with-slots (%session) session
    (let ((%channel (ssh-channel-new %session)))
      (core:abort-protect
          (progn
            (ssh-channel-open-session %channel %session)
            (make-instance 'channel :%channel %channel
                                    :%session %session))
        (unwind-protect
             (ssh-channel-close %channel %session)
          (ssh-channel-free %channel))))))

(defun close-channel (channel)
  (declare (type channel channel))
  (with-slots (%channel %session) channel
    (unwind-protect
         (ssh-channel-close %channel %session)
      (ssh-channel-free %channel))))

(defmacro with-channel ((channel session &rest args) &body body)
  `(let ((,channel (open-channel ,session ,@args)))
     (unwind-protect
          (progn
            ,@body)
       (close-channel ,channel))))

(defun execute-command (command channel)
  (declare (type string command)
           (type channel channel))
  (let ((block-size 1024))
    (with-slots (%channel %session) channel
      (ssh-channel-request-exec %channel %session command)
      (flet ((read-output (buffer stderr)
               (declare (type core:buffer buffer)
                        (type boolean stderr))
               (let ((offset (core:buffer-reserve buffer block-size)))
                 (ffi:with-pinned-vector-data
                     (%buffer (core:buffer-data buffer) offset)
                   (let ((nb-read (ssh-channel-read %channel %session %buffer
                                                    block-size stderr)))
                     (incf (core:buffer-end buffer) nb-read)
                     nb-read)))))
        (do ((stdout (core:make-buffer block-size))
             (stdout-closed-p nil)
             (stderr (core:make-buffer block-size))
             (stderr-closed-p nil))
            ((and stdout-closed-p stderr-closed-p)
             (values (ssh-channel-get-exit-status %channel %session)
                     (text:decode-string (core:buffer-content stdout))
                     (text:decode-string (core:buffer-content stderr))))
          (unless stdout-closed-p
            (setf stdout-closed-p (zerop (read-output stdout nil))))
          (unless stderr-closed-p
            (setf stderr-closed-p (zerop (read-output stderr t)))))))))
