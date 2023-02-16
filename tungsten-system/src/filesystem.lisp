(in-package :system)

(defun read-file (path &key external-format)
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type 'core:octet :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type 'core:octet)
      (loop
        (let* ((capacity (array-total-size data))
               (nb-left (- capacity offset)))
          (when (< nb-left block-size)
            (let ((new-length (max (+ capacity (- block-size nb-left))
                                   (floor (* capacity 3) 2))))
              (setf data (adjust-array data new-length)))))
        (let ((end (read-sequence data file :start offset)))
          (when (= end offset)
            (let ((data (adjust-array data end)))
              (return-from read-file
                (if external-format
                    (let ((encoding
                            (text:external-format-encoding external-format)))
                      (text:decode-string data :encoding encoding))
                    data))))
          (setf offset end))))))
