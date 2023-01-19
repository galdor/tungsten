(in-package :http)

(deftype route-path-segment ()
  '(or string symbol))

(deftype route-path ()
  'list)

(defclass route ()
  ((name
    :type symbol
    :initarg :name
    :reader route-name)
   (method
    :type (or request-method null)
    :initarg :method
    :initform nil
    :reader route-method)
   (path
    :type (or route-path null)
    :initarg :path
    :initform nil
    :reader route-path)
   (request-handler
    :type (or symbol function)
    :initarg :request-handler
    :accessor route-request-handler)))

(defun route-path-segment-equal (segment1 segment2)
  (cond
    ((and (stringp segment1) (stringp segment2))
     (string= segment1 segment2))
    ((and (symbolp segment1) (symbolp segment2))
     (eq segment1 segment2))
    (t
     nil)))

(defun route-path-equal (path1 path2)
  (and (= (length path1) (length path2))
       (every 'route-path-segment-equal path1 path2)))

(defun parse-route-path (string)
  (declare (type string string))
  (when (= (length string) 0)
    (error "empty route path"))
  (unless (char= (char string 0) #\/)
    (error "route path does not start with a ~S" #\/))
  (do ((segments nil)
       (start 1)
       (end (if (char= (char string (1- (length string))) #\/)
                (1- (length string))
                (length string))))
      ((> start end)
       (nreverse segments))
    (let* ((slash (position #\/ string :start start :end end))
           (segment-end (or slash end))
           (segment (cond
                      ((= start segment-end)
                       (error "invalid empty route path segment"))
                      ((string= string "*" :start1 start :end1 segment-end)
                       (unless (= segment-end end)
                         (error "wildcard can only be used for the last ~
                                 path segment"))
                       :*)
                      ((char= (char string start) #\:)
                       (when (string= string ":*"
                                      :start1 start :end1 segment-end)
                         (error "reserved path variable name \":*"))
                       (intern
                        (string-upcase (subseq string (1+ start) segment-end))
                        :keyword))
                      (t
                       (subseq string start segment-end)))))
      (push segment segments)
      (setf start (1+ segment-end)))))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type t)
    (princ (route-name route) stream)))

(defun match-route (route request)
  (declare (type route route)
           (type request request))
  (when (match-route-method route (request-method request))
    (match-route-path route (request-target request))))

(defun match-route-method (route method)
  (declare (type route route)
           (type request-method method))
  (if (route-method route)
      (request-method-equal (route-method route) method)
      t))

(defun match-route-path (route target)
  (declare (type route route)
           (type uri:uri target))
  (do ((route-segments (route-path route) (cdr route-segments))
       (request-segments (uri:uri-path-segments target) (cdr request-segments))
       (path-variables nil))
      ((and (null request-segments) (null route-segments))
       (values t (nreverse path-variables)))
    (let ((route-segment (car route-segments))
          (request-segment (car request-segments)))
      (cond
        ((and (stringp route-segment) (equal route-segment request-segment))
         nil)
        ((and (eq route-segment :*) request-segments)
         (push (cons :* (format nil "~{~A~^/~}" request-segments))
               path-variables)
         (setf request-segments nil))
        ((and route-segment (symbolp route-segment) request-segment)
         (push (cons route-segment request-segment) path-variables))
        (t
         (return-from match-route-path nil))))))
