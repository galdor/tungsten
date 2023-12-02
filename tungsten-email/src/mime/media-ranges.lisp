(in-package :mime)

;;; Reference: RFC 9110
;;;
;;; It might seem strange not to put this in tungsten-http, but media ranges
;;; are used in other standards such as OpenAPI 3.x specifications where they
;;; are named "media type ranges". Ultimately, media ranges are a simple
;;; extension to media types and fit it just fine in tungsten-mime.

(define-condition invalid-media-range (parse-error)
  ((description
    :type string
    :initarg :description
    :reader invalid-media-range-description))
  (:report
   (lambda (condition stream)
     (format stream "invalid MIME media range: ~A"
             (invalid-media-range-description condition)))))

(defclass media-range ()
  ((type
    :type (or string (member *))
    :initarg :type
    :accessor media-range-type)
   (subtype
    :type (or string (member *))
    :initarg :subtype
    :accessor media-range-subtype)
   (parameters
    :type list
    :initarg :parameters
    :initform nil
    :accessor media-range-parameters)))

(defmethod print-object ((media-range media-range) stream)
  (print-unreadable-object (media-range stream :type t)
    (prin1 (serialize-media-range media-range) stream)))

(defmethod initialize-instance :after ((media-range media-range)
                                       &key &allow-other-keys)
  (with-slots (type subtype) media-range
    (when (and (eq type '*) (not (eq subtype '*)))
      (error 'invalid-media-range
             :description "invalid wildcard type with non-wildcard subtype"))))

(defun make-media-range (type subtype &key parameters)
  (declare (type (or string (member *)) type subtype)
           (type list parameters))
  (make-instance 'media-range :type type :subtype subtype
                              :parameters parameters))

(defun media-range (media-range)
  (declare (type (or media-range string) media-range))
  (etypecase media-range
    (media-range media-range)
    (string (parse-media-range media-range))))

(defun media-range-parameter (media-range name)
  (declare (type media-range media-range))
  (cdr (assoc name (media-range-parameters media-range) :test #'equalp)))

(defun (setf media-range-parameter) (value media-range name)
  (declare (type media-range media-range))
  (with-slots (parameters) media-range
    (let ((parameter (assoc name parameters :test #'equalp)))
      (if parameter
          (rplacd parameter value)
          (push (cons name value) parameters)))))

(defun serialize-media-range (media-range)
  (declare (type media-range media-range))
  (with-output-to-string (stream)
    (with-slots (type subtype parameters) media-range
      (case type
        (* (write-char #\* stream))
        (t (write-string type stream)))
      (write-char #\/ stream)
      (case subtype
        (* (write-char #\* stream))
        (t (write-string subtype stream)))
      (dolist (parameter parameters)
        (write-char #\; stream)
        (write-string (car parameter) stream)
        (write-char #\= stream)
        (write-string (cdr parameter) stream)))))

(defun parse-media-range (string &key (start 0) end)
  (declare (type string string)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (handler-case
      (let* ((end (or end (length string)))
             (media-type (parse-media-type string :start start :end end)))
        (with-slots (type subtype parameters) media-type
          (make-instance 'media-range
                         :type (if (string= type "*") '* type)
                         :subtype (if (string= subtype "*") '* subtype)
                         :parameters parameters)))
    (invalid-media-type (condition)
      (with-slots (description) condition
        (error 'invalid-media-range :description description)))))

(defun normalize-media-range (media-range)
  (declare (type media-range media-range))
  (with-slots (type subtype parameters) media-range
    (setf type (case type
                 (* '*)
                 (t (string-downcase type))))
    (setf subtype (case type
                    (* '*)
                    (t (string-downcase subtype))))
    (dolist (parameter parameters)
      (rplaca parameter (string-downcase (car parameter))))
    media-range))

(defun match-media-range (media-range media-type)
  "Check if MEDIA-RANGE matches MEDIA-TYPE and return :EXACT if both type and
subtype are the same, PARTIAL if types are the same and subtypes match due to
a wildcard, WILDCARD if both types and subtypes match due to a wildcard, or
NIL if there is no match."
  (declare (type media-range media-range)
           (type media-type media-type))
  (let ((wildcard-type-p (eq (media-range-type media-range) '*))
        (wildcard-subtype-p (eq (media-range-subtype media-range) '*)))
    (when (or wildcard-type-p
              (string= (media-range-type media-range)
                       (media-type-type media-type)))
      (when (or wildcard-subtype-p
                (string= (media-range-subtype media-range)
                         (media-type-subtype media-type)))
        (cond
          ((and wildcard-type-p wildcard-subtype-p)
           :wildcard)
          ((or wildcard-type-p wildcard-subtype-p)
           :partial)
          (t
           :exact))))))

(defun match-media-ranges (media-ranges media-type &key (key #'identity))
  "Return a list of the media ranges in MEDIA-RANGES which match MEDIA-TYPE
sorted by descending exactness."
  (let ((matches nil))
    (dolist (media-range media-ranges)
      (let ((match (match-media-range (funcall key media-range) media-type)))
        (when match
          (push (cons media-range match) matches))))
    (flet ((rank (match)
             (ecase (cdr match)
               (:exact 1)
               (:partial 2)
               (:wildcard 3))))
      (mapcar #'car (sort matches #'< :key #'rank)))))
