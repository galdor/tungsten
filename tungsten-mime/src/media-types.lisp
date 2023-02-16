(in-package :mime)

;;; Reference: RFC 2045

(define-condition invalid-media-type (parse-error)
  ((description
    :type string
    :initarg :description
    :reader invalid-media-type-description))
  (:report
   (lambda (condition stream)
     (with-slots (description) condition
       (format stream "Invalid MIME media type: ~A." description)))))

(defun invalid-media-type (format &rest arguments)
  (error 'invalid-media-type
         :description (apply #'format nil format arguments)))

(defclass media-type ()
  ((type
    :type string
    :initarg :type
    :accessor media-type-type)
   (subtype
    :type string
    :initarg :subtype
    :accessor media-type-subtype)
   (parameters
    :type list
    :initarg :parameters
    :initform nil
    :accessor media-type-parameters)))

(defmethod print-object ((media-type media-type) stream)
  (print-unreadable-object (media-type stream :type t)
    (prin1 (serialize-media-type media-type) stream)))

(defun make-media-type (type subtype &key parameters)
  (declare (type string type subtype)
           (type list parameters))
  (make-instance 'media-type :type type :subtype subtype
                             :parameters parameters))

(defun media-type (media-type)
  (declare (type (or media-type string) media-type))
  (etypecase media-type
    (media-type media-type)
    (string (parse-media-type media-type))))

(defun media-type-parameter (media-type name)
  (declare (type media-type media-type))
  (cdr (assoc name (media-type-parameters media-type) :test #'equalp)))

(defun (setf media-type-parameter) (value media-type name)
  (declare (type media-type media-type))
  (with-slots (parameters) media-type
    (let ((parameter (assoc name parameters :test #'equalp)))
      (if parameter
          (rplacd parameter value)
          (push (cons name value) parameters)))))

(defun serialize-media-type (media-type)
  (declare (type media-type media-type))
  (with-output-to-string (stream)
    (with-slots (type subtype parameters) media-type
      (write-string type stream)
      (write-char #\/ stream)
      (write-string subtype stream)
      (dolist (parameter parameters)
        (write-char #\; stream)
        (write-string (car parameter) stream)
        (write-char #\= stream)
        (write-string (cdr parameter) stream)))))

(defun parse-media-type (string &key (start 0) (end (length string)))
  (declare (type string string)
           (type (integer 0) start end))
  (let* ((media-type (make-instance 'media-type))
         (slash (position #\/ string :start start :end end))
         (hspace #.(format nil "~C~C" #\Space #\Tab)))
    ;; Type
    (unless slash
      (invalid-media-type "missing '/' separator"))
    (let ((type (string-trim hspace (subseq string start slash))))
      (when (= (length type) 0)
        (invalid-media-type "empty type"))
      (setf (media-type-type media-type) type))
    (setf start (1+ slash))
    ;; Subtype
    (let* ((subtype-end (or (position #\; string :start start :end end) end))
           (subtype (string-trim hspace (subseq string start subtype-end))))
      (when (= (length subtype) 0)
        (invalid-media-type "empty subtype"))
      (setf (media-type-subtype media-type) subtype)
      (setf start subtype-end))
    ;; Parameters
    (do ((parameters nil))
        ((>= start end)
         (setf (media-type-parameters media-type) (nreverse parameters)))
      (incf start) ; #\;
      (let ((equal (position #\= string :start start :end end))
            (name nil)
            (value nil))
        (unless equal
          (invalid-media-type "missing '=' separator"))
        (setf name (string-trim hspace (subseq string start equal)))
        (when (= (length name) 0)
          (invalid-media-type "empty parameter name"))
        (setf start (1+ equal))
        (let ((value-end (or (position #\; string :start start :end end) end)))
          (setf value (string-trim hspace (subseq string start value-end)))
          (when (= (length value) 0)
            (invalid-media-type "empty parameter value"))
          (setf start value-end))
        (push (cons name value) parameters)))
    ;; End
    media-type))

(defun normalize-media-type (media-type)
  (declare (type media-type media-type))
  (with-slots (type subtype parameters) media-type
    (setf type (string-downcase type))
    (setf subtype (string-downcase subtype))
    (dolist (parameter parameters)
      (rplaca parameter (string-downcase (car parameter))))
    media-type))
