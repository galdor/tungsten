(in-package :mime)

;;; The Quoted-Printable encoding is defined in RFC 2045 (Multipurpose
;;; Internet Mail Extensions (MIME) Part One: Format of Internet Message
;;; Bodies).
;;;
;;; Quoted-Printable encoding transforms a sequence of octets into a sequence
;;; of ASCII characters, but input is not arbitrary binary data: it is
;;; supposed to be text. As such, we use a string as input and convert
;;; non-ASCII characters using the current value of TEXT:*DEFAULT-ENCODING*.
;;;
;;; RFC 2045 expects line breaks in the input text to be represented as the
;;; CRLF sequence. In practice, when composing a message, developers will
;;; provide text using the newline representation of their platform. We should
;;; support multiple representations, but for the time being the usual UNIX
;;; #\NEWLINE character will do just fine. Of course we convert it to the CRLF
;;; sequence in the output string.

(defun encode-quoted-printable (string stream
                                &key (start 0) (end (length string))
                                     (max-line-length 76))
  (declare (type string string)
           (type stream stream)
           (type (integer 0) start end)
           (type (integer 4) max-line-length))
  (let ((line-length 0))
    (labels ((whitespacep (c)
               (declare (type character c))
               (or (char= c #\Tab) (char= c #\Space)))
             (crlfp (c)
               (declare (type character c))
               (or (char= c #\Return) (char= c #\Newline)))
             (emit-eol ()
               (write-string (text:eol-string :crlf) stream)
               (setf line-length 0))
             (emit-char (c)
               (declare (type character c))
               (when (> (1+ line-length) max-line-length)
                 (write-char #\= stream)
                 (emit-eol))
               (write-char c stream)
               (incf line-length))
             (emit-string (s)
               (declare (type string s))
               (when (> (+ line-length (length s) 1) max-line-length)
                 (write-char #\= stream)
                 (emit-eol))
               (write-string s stream)
               (setf line-length (length s))))
      (do ((i start (1+ i)))
          ((>= i end))
        (let ((c (char string i)))
          (cond
            ((or (char<= #\! c #\<) (char<= #\> c #\~))
             (emit-char c))
            ((char= c #\Newline)
             (emit-eol))
            ((and (whitespacep c)
                  (let ((nextc (find-if-not #'whitespacep string
                                            :start (1+ i) :end end)))
                    (and nextc (not (crlfp nextc)))))
             (emit-char c))
            (t
             (let ((octets (text:encode-string (string c))))
               (dotimes (j (length octets))
                 (emit-string
                  (format nil "=~2,'0X" (aref octets j))))))))))))
