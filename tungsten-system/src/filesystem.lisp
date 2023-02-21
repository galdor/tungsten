(in-package :system)

(defun directory-path (path)
  "Return a pathname referencing the directory at PATH."
  (declare (type (or pathname string) path))
  (make-pathname :directory (append (or (pathname-directory path)
                                        (list :relative))
                                    (list (file-namestring path)))
                 :name nil :type nil :defaults path))

(defun read-file (path &key external-format (if-does-not-exist :error))
  (declare (type (or pathname string) path))
  (let ((data (make-array 0 :element-type 'core:octet :adjustable t))
        (block-size 4096)
        (offset 0))
    (with-open-file (file path :element-type 'core:octet
                               :if-does-not-exist if-does-not-exist)
      (when (null file)
        (return-from read-file nil))
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
