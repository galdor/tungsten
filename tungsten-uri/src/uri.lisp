(in-package :uri)

(deftype port-number ()
  '(integer 1 65535))

(defclass uri ()
  ((scheme
    :type (or null string)
    :initarg :scheme
    :accessor uri-scheme)
   (username
    :type (or null string)
    :initarg :username
    :accessor uri-username)
   (password
    :type (or null string)
    :initarg :password
    :accessor uri-password)
   (host
    :type (or null string)
    :initarg :host
    :accessor uri-host)
   (port
    :type (or null port-number)
    :initarg :port
    :accessor uri-port)
   (path
    :type (or null string)
    :initarg :path
    :accessor uri-path)
   (query
    :type (or null list)
    :initarg :query
    :accessor uri-query)
   (fragment
    :type (or null string)
    :initarg :fragment
    :accessor uri-fragment)))

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t)
    (write-string (serialize uri) stream)))

(defun make-uri (&key scheme username password host port path query fragment)
  (make-instance 'uri :scheme scheme
                      :username username
                      :password password
                      :host host
                      :port port
                      :path path
                      :query query
                      :fragment fragment))

(defun copy-uri (uri)
  (make-uri :scheme (uri-scheme uri)
            :username (uri-username uri)
            :password (uri-password uri)
            :host (uri-host uri)
            :port (uri-port uri)
            :path (uri-path uri)
            :query (uri-query uri)
            :fragment (uri-fragment uri)))

(defun uri (uri)
  (declare (type (or uri string) uri))
  (etypecase uri
    (uri uri)
    (string (parse uri))))

(defun uri-absolute-path-p (uri)
  (declare (type uri uri))
  (with-slots (path) uri
    (when path
      (absolute-path-p path))))

(defun absolute-path-p (path)
  (declare (type string path))
  (and path
       (> (length path) 0)
       (char= (char path 0) #\/)))

(defun uri-path-segments (uri)
  (declare (type uri uri))
  (with-slots (path) uri
    (when path
      (path-segments path))))

(defun path-segments (path)
  (declare (type string path))
  (do ((start (if (absolute-path-p path) 1 0))
       (end (length path))
       (segments nil))
      ((>= start end)
       (nreverse segments))
    (let* ((slash (position #\/ path :start start :end end))
           (segment-end (or slash end)))
      (push (subseq path start segment-end) segments)
      (setf start (1+ segment-end)))))
