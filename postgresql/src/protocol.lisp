(in-package :postgresql)

(defun write-startup-message (major-version minor-version parameters stream)
  (declare (type (signed-byte 16) major-version minor-version)
           (type list parameters)
           (type stream stream))
  (write-message `((int16 ,major-version)
                   (int16 ,minor-version)
                   ,@(mapcar (lambda (parameter)
                               `((string ,(car parameter))
                                 (string,(cdr parameter))))
                             parameters)
                   (int8 0))
                 stream))

(defun write-message (value stream)
  (let ((data (make-array 32 :element-type 'core:octet
                             :adjustable t :fill-pointer 4)))
    (labels
        ((reserve (size)
           (let* ((capacity (length data))
                  (position (fill-pointer data))
                  (required-size (+ position size)))
             (when (> required-size capacity)
               (let ((new-capacity (max required-size (* capacity 2))))
                 (setf data (adjust-array data new-capacity))
                 (incf (fill-pointer data) size)
                 position))))
         (encode-value (value)
           (cond
             ((and (listp value) (listp (car value)))
              (mapc #'encode-value value))
             (t
              (let ((type (car value))
                    (value (cadr value)))
                (ecase type
                  (int8
                   (setf (core:binref :int8 data (reserve 1)) value))
                  (int16
                   (setf (core:binref :int16be data (reserve 2)) value))
                  (int32
                   (setf (core:binref :int32be data (reserve 4)) value))
                  (string
                   (let* ((nb-octets (text:encoded-string-length value))
                          (offset (reserve (1+ nb-octets))))
                     (text:encode-string value
                                         :octets data
                                         :offset offset
                                         :nb-octets nb-octets)
                     (setf (aref data (+ offset nb-octets)) 0)))
                  (octets
                   (let* ((value (cadr value))
                          (nb-octets (length value))
                          (offset (reserve nb-octets)))
                     (replace data value :start1 offset)))))))))
      (encode-value value))
    (setf (core:binref :int32be data 0) (fill-pointer data))
    (write-sequence data stream)))
