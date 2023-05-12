(in-package :system)

(defun make-pipe (&key (input-external-format text:*default-external-format*)
                       (output-external-format text:*default-external-format*))
  (multiple-value-bind (input-fd output-fd)
      (pipe)
    (core:abort-protect
        (let ((input-stream
                (make-instance 'input-io-stream
                               :fd input-fd
                               :external-format input-external-format))
              (output-stream
                (make-instance 'output-io-stream
                               :fd output-fd
                               :external-format output-external-format)))
          (values input-stream output-stream))
      (ignore-errors
       (close-fd input-fd)
       (close-fd output-fd)))))
