(in-package :system)

(defun read-file (path)
  (declare (type (or pathname string) path))
  (with-open-file (stream path :element-type 'core:octet)
    (let* ((size (file-length stream))
           (data (make-array size :element-type 'core:octet)))
      (read-sequence data stream)
      data)))

(defun read-file-string (path
                         &key (external-format text:*default-external-format*))
  (declare (type (or pathname string) path))
  (let ((encoding (text:external-format-encoding external-format)))
    (text:decode-string (read-file path) :encoding encoding)))
