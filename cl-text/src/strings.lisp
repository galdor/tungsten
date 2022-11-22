(in-package :text)

(defun encode-string (string &key (encoding *default-encoding*) start end)
  (declare (type simple-string string)
           (type (or index null) end))
  (let* ((encoding (encoding encoding))
         (nb-octets
           (funcall (encoding-encoded-string-length-function encoding)
                    string start end))
         (octets (make-array nb-octets :element-type 'octet)))
    (funcall (encoding-encoding-function encoding) string start end octets 0)))

(defun decode-string (octets &key (encoding *default-encoding*) start end)
  (declare (type octet-vector octets)
           (type (or index null) start end))
  (let* ((encoding (encoding encoding))
         (nb-characters
           (funcall (encoding-decoded-string-length-function encoding)
                    octets start end))
         (string (make-array nb-characters :element-type 'character)))
    (funcall (encoding-decoding-function encoding) octets start end string 0)))
