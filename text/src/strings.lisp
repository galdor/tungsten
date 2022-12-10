(in-package :text)

(defun encoded-string-length (string &key (encoding *default-encoding*)
                                          start end)
  (declare (type simple-string string)
           (type (or index null) end))
  (let ((encoding (encoding encoding)))
    (funcall (encoding-encoded-string-length-function encoding)
             string start end)))

(defun encode-string (string &key (encoding *default-encoding*) start end
                                  octets (offset 0))
  (declare (type simple-string string)
           (type (or index null) end))
  (let* ((encoding (encoding encoding))
         (nb-octets
           (funcall (encoding-encoded-string-length-function encoding)
                    string start end)))
    (unless octets
      (setf octets (make-array nb-octets :element-type 'octet)))
    (funcall (encoding-encoding-function encoding) string start end
             octets offset)
    octets))

(defun decoded-string-length (octets &key (encoding *default-encoding*)
                                          start end)
  (declare (type octet-vector octets)
           (type (or index null) end))
  (let ((encoding (encoding encoding)))
    (funcall (encoding-decoded-string-length-function encoding)
             octets start end)))

(defun decode-string (octets &key (encoding *default-encoding*) start end
                                  string (offset 0))
  (declare (type octet-vector octets)
           (type (or index null) start end))
  (let* ((encoding (encoding encoding))
         (nb-characters
           (funcall (encoding-decoded-string-length-function encoding)
                    octets start end)))
    (unless string
      (setf string (make-array nb-characters :element-type 'character)))
    (funcall (encoding-decoding-function encoding) octets start end
             string offset)))