(in-package :text)

(defun encoded-character-length (character &key (encoding *default-encoding*))
  (declare (type character character))
  (let ((encoding (encoding encoding)))
    (funcall (encoding-encoded-character-length-function encoding) character)))

(defun encoded-string-length (string &key (encoding *default-encoding*)
                                          start end)
  (declare (type simple-string string)
           (type (or index null) end))
  (let ((encoding (encoding encoding)))
    (funcall (encoding-encoded-string-length-function encoding)
             string start end)))

(defun encode-string (string &key (encoding *default-encoding*) start end
                                  octets (offset 0)
                                  nb-octets)
  (declare (type simple-string string)
           (type (or index null) end))
  (let* ((encoding (encoding encoding))
         (nb-octets
           (or nb-octets
               (funcall (encoding-encoded-string-length-function encoding)
                        string start end)))
         (encode-character (encoding-character-encoding-function encoding)))
    (unless octets
      (setf octets (make-array nb-octets :element-type 'core:octet)))
    (do ((end (or end (length string)))
         (i (or start 0) (1+ i))
         (j offset))
        ((>= i end)
         octets)
      (incf j (funcall encode-character (schar string i) octets j)))))

(defun decoded-string-length (octets &key (encoding *default-encoding*)
                                          start end)
  (declare (type core:octet-vector octets)
           (type (or index null) end))
  (let ((encoding (encoding encoding)))
    (funcall (encoding-decoded-string-length-function encoding)
             octets start end)))

(defun decode-character (octets &key (encoding *default-encoding*) start end)
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let* ((encoding (encoding encoding))
         (decode-character (encoding-character-decoding-function encoding)))
    (funcall decode-character octets start end)))

(defun decode-string (octets &key (encoding *default-encoding*) start end
                                  string (offset 0))
  (declare (type core:octet-vector octets)
           (type (or index null) start end))
  (let* ((encoding (encoding encoding))
         (nb-characters
           (funcall (encoding-decoded-string-length-function encoding)
                    octets start end))
         (decode-character (encoding-character-decoding-function encoding)))
    (unless string
      (setf string (make-array nb-characters :element-type 'character)))
    (do ((end (or end (length octets)))
         (i (or start 0))
         (j offset (1+ j)))
        ((>= i end)
         string)
      (multiple-value-bind (character nb-octets)
          (funcall decode-character octets i end)
        (when (null character)
          (return string))
        (setf (schar string j) character)
        (incf i nb-octets)))))