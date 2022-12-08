(in-package :uri)

(defun serialize (uri)
  (with-slots (scheme username password host port path query fragment) uri
    (with-output-to-string (stream)
      (when scheme
        (serialize-scheme scheme stream))
      (when host
        (serialize-authority username password host port stream))
      (when path
        (serialize-path path stream))
      (when query
        (serialize-query query stream))
      (when fragment
        (serialize-fragment fragment stream)))))

(defun serialize-scheme (scheme stream)
  (write-string scheme stream)
  (write-char #\: stream))

(defun serialize-authority (username password host port stream)
  (write-string "//" stream)
  (when (or username password)
    (serialize-user-info username password stream)
    (write-char #\@ stream))
  (serialize-host host stream)
  (when port
    (write-char #\: stream)
    (princ port stream)))

(defun serialize-user-info (username password stream)
  (cond
    ((and username password)
     (write-string (encode-username username) stream)
     (write-char #\: stream)
     (write-string (encode-password password) stream))
    (username
     (write-string (encode-username username) stream))
    (password
     (write-char #\: stream)
     (write-string (encode-password password) stream))))

(defun serialize-host (host stream)
  (cond
    ((find #\: host)
     (write-char #\[ stream)
     (write-string host stream)
     (write-char #\] stream))
    (t
     (write-string host stream))))

(defun serialize-path (path stream)
  (write-string (encode-path path) stream))

(defun serialize-query (query stream)
  (write-char #\? stream)
  (write-string (encode-query query) stream))

(defun serialize-fragment (fragment stream)
  (write-char #\# stream)
  (write-string (encode-fragment fragment) stream))
