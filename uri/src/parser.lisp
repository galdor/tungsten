(in-package :uri)

(define-condition uri-parse-error (parse-error)
  ())

(define-condition truncated-host (uri-parse-error)
  ((host
    :type string
    :initarg :host))
  (:report
   (lambda (c stream)
     (format stream "Truncated host ~S." (slot-value c 'host)))))

(define-condition invalid-host (uri-parse-error)
  ((host
    :type string
    :initarg :host))
  (:report
   (lambda (c stream)
     (format stream "Invalid host ~S." (slot-value c 'host)))))

(define-condition invalid-port (uri-parse-error)
  ((port
    :type (or string number)
    :initarg :port))
  (:report
   (lambda (c stream)
     (with-slots (port) c
       (format stream "Invalid port ~?."
               (etypecase port
                 (string "~S")
                 (integer "~D"))
               (list port))))))

(defun parse (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end))
  (let ((uri (make-uri))
        (i start))
    ;; Scheme
    (when (and (< i end) (char/= (char string i) #\/))
      (let ((colon (position #\: string :start i :end end)))
        (when colon
          (setf (uri-scheme uri) (subseq string i colon))
          (setf i (1+ colon)))))
    ;; Authority
    (when (and (>= (- end i) 2)
               (char= (char string i) #\/)
               (char= (char string (1+ i)) #\/))
      (incf i 2)
      (let* ((separator (position-if (lambda (c)
                                       (or (char= c #\/)
                                           (char= c #\?)
                                           (char= c #\#)))
                                     string :start i :end end))
             (authority-end (or separator end)))
        (parse-authority string uri :start i :end authority-end)
        (setf i authority-end)))
    ;; Path
    (let* ((separator (position-if (lambda (c)
                                     (or (char= c #\?)
                                         (char= c #\#)))
                                   string :start i :end end))
           (path-end (or separator end)))
      (when (> (- path-end i) 0)
        (setf (uri-path uri) (percent-decode string :start i :end path-end)))
      (setf i path-end))
    ;; Query
    (when (and (< i end) (char= (char string i) #\?))
      (let* ((separator (position #\# string :start i :end end))
             (query-end (or separator end)))
        (setf (uri-query uri)
              (parse-query string :start (1+ i) :end query-end))
        (setf i query-end)))
    ;; Fragment
    (when (and (< i end) (char= (char string i) #\#))
      (incf i)
      (setf (uri-fragment uri)
            (percent-decode string :start i :end end)))
    uri))

(defun parse-authority (string uri &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end))
  (let ((at (position #\@ string :start start :end end)))
    (cond
      (at
       ;; User info
       (let ((colon (position #\: string :start start :end at)))
         (setf (uri-username uri)
               (percent-decode string :start start :end (or colon at)))
         (when colon
           (setf (uri-password uri)
                 (percent-decode string :start (1+ colon) :end at))))
       ;; Host and port
       (parse-host-and-port string uri :start (1+ at) :end end))
      (t
       ;; Host and port only
       (parse-host-and-port string uri :start start :end end)))))

(defun parse-host-and-port (string uri &key (start 0) (end (length string)))
  (cond
    ((and (< start end)
          (char= (char string start) #\[))
     ;; IPv6 address
     (let ((closing-bracket (position #\] string :start (1+ start) :end end)))
       (unless closing-bracket
         (error 'truncated-host :host (subseq string start end)))
       (setf (uri-host uri) (subseq string (1+ start) closing-bracket))
       (when (< closing-bracket (1- end))
         (unless (char= (char string (1+ closing-bracket)) #\:)
           (error 'invalid-host :host (subseq string start end)))
         (setf (uri-port uri)
               (parse-port string :start (+ closing-bracket 2)
                                  :end end)))))
    (t
     ;; Hostname or IPv4 address
     (let ((colon (position #\: string :start start :end end)))
       (cond
         (colon
          (setf (uri-host uri) (subseq string start colon))
          (setf (uri-port uri) (parse-port string :start (1+ colon) :end end)))
         (t
          (setf (uri-host uri) (subseq string start end))))))))

(defun parse-port (string &key (start 0) (end (length string)))
  (let ((port (handler-case
                  (parse-integer string :start start :end end)
                (parse-error ()
                  (error 'invalid-port :port (subseq string start end))))))
    (unless (< 0 port 65536)
      (error 'invalid-port :port port))
    port))

(defun parse-query (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end))
  (do ((query nil)
       (i (or start 0)))
      ((>= i end)
       (nreverse query))
    (let ((ampersand (or (position #\& string :start i :end end) end)))
      (push (parse-query-parameter string :start i :end ampersand) query)
      (setf i (1+ ampersand)))))

(defun parse-query-parameter (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type integer start end))
  (let* ((equal (or (position #\= string :start start :end end) end))
         (name (percent-decode string :start start :end equal
                                      :decode-plus t))
         (value (percent-decode string :start (1+ equal) :end end
                                       :decode-plus t)))
    (cons name value)))
