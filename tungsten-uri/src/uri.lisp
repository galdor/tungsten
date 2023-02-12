(in-package :uri)

;;; Reference: RFC3986
;;;
;;; In theory we need to handle differently URIs and relative references. The
;;; obvious representation would be to have a URI-REFERENCE base class, from
;;; which URI and RELATIVE-REFERENCE inherit. But then we end up with either
;;; two sets of functions, or functions which manipulate URI-REFERENCE objects
;;; with long names which make no sense to most people.
;;;
;;; But the only actual difference between a URI and a relative reference is
;;; that a URI must have a scheme, nothing more. Thus there is no point in
;;; making things complicated. We provide URI-RELATIVE-PATH-P to differentiate
;;; them, but use the same URI class for both. Ruby acts similarly, with
;;; different classes for different schemes, but making the distinction with
;;; the "relative?" method. Erlang is even simpler, "uri_string:parse/1"
;;; always return a value of type "uri_map()" and does not distinguish between
;;; URIs and relative references.

(deftype port-number ()
  '(integer 1 65535))

(defclass uri ()
  ((scheme
    :type (or null string)
    :initarg :scheme
    :initform nil
    :accessor uri-scheme)
   (username
    :type (or null string)
    :initarg :username
    :initform nil
    :accessor uri-username)
   (password
    :type (or null string)
    :initarg :password
    :initform nil
    :accessor uri-password)
   (host
    :type (or null string)
    :initarg :host
    :initform nil
    :accessor uri-host)
   (port
    :type (or null port-number)
    :initarg :port
    :initform nil
    :accessor uri-port)
   (path
    :type (or null string)
    :initarg :path
    :initform nil
    :accessor uri-path)
   (query
    :type (or null list)
    :initarg :query
    :initform nil
    :accessor uri-query)
   (fragment
    :type (or null string)
    :initarg :fragment
    :initform nil
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

(defun uri-relative-reference-p (uri)
  (declare (type uri uri))
  (null (uri-scheme uri)))

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
