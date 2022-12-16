(in-package :system)

(defclass stream-socket (streams:fundamental-binary-input-stream
                         streams:fundamental-binary-output-stream)
  ((file-descriptor
    :type (or (integer 0) null)
    :initarg :file-descriptor
    :initform nil
    :reader stream-socket-file-descriptor)
   (address
    :type socket-address
    :initarg :address
    :reader stream-socket-address)))

(defmethod close ((socket stream-socket) &key abort)
  (declare (ignore abort))
  (with-slots (file-descriptor) socket
    (when file-descriptor
      (close-fd file-descriptor)
      (setf file-descriptor nil)
      t)))

(defmethod open-stream-p ((socket stream-socket))
  (not (null (stream-socket-file-descriptor socket))))
