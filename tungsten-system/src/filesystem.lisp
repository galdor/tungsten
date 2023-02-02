(in-package :system)

(defun read-file (path)
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type 'core:octet :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type 'core:octet)
      (loop
        (let* ((capacity (array-total-size data))
               (nb-left (- capacity offset)))
          (when (< nb-left block-size)
            (setf data
                  (adjust-array data (+ capacity (- block-size nb-left))))))
        (let ((end (read-sequence data file :start offset)))
          (when (= end offset)
            (return-from read-file (adjust-array data end)))
          (setf offset end))))))

(defun read-file-string (path
                         &key (external-format text:*default-external-format*))
  (declare (type (or pathname string) path))
  (let ((encoding (text:external-format-encoding external-format)))
    (text:decode-string (read-file path) :encoding encoding)))
