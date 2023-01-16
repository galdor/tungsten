(in-package :log)

(defparameter *default-stream*
  *error-output*)

(defvar *logger* nil)

(defclass logger ()
  ((domain
    :type domain
    :initarg :domain
    :initform nil
    :accessor logger-domain)
   (data
    :type list
    :initarg :data
    :accessor logger-data)
   (sink
    :type sink
    :initarg :sink
    :initform (make-default-sink)
    :accessor logger-sink)))

(defmethod print-object ((logger logger) stream)
  (print-unreadable-object (logger stream :type t)
    (with-slots (domain) logger
      (let ((*print-case* :downcase))
        (format stream "~{~A~^.~}" domain)))))

(defun make-logger (name &key parent data sink)
  (declare (type domain-part name)
           (type (or logger null) parent)
           (type list data)
           (type (or null sink) sink))
  (flet ((inherit (slot &optional default)
           (if parent
               (slot-value parent slot)
               default)))
    (make-instance 'logger
                   :domain (append (inherit 'domain) (list name))
                   :data (append data (inherit 'data))
                   :sink (or sink (inherit 'sink (make-default-sink))))))

(defmacro with-logger ((name &key data sink) &body body)
  `(let ((*logger* (make-logger ,name :parent *logger*
                                      :data ,data
                                      :sink ,sink)))
     ,@body))

(defun log-message (message &key (logger *logger*))
  (declare (type message message)
           (type logger logger))
  (with-slots (domain data sink) logger
    (unless (message-domain message)
      (setf (message-domain message) domain))
    (setf (message-data message) (append (message-data message) data))
    (write-message message sink)))

(defun log-debug (format &rest args)
  (log-message
   (make-instance 'message :level :debug
                           :text (apply #'format nil format args))))

(defun log-debug-data (data format &rest args)
  (log-message
   (make-instance 'message :level :debug
                           :text (apply #'format nil format args)
                           :data data)))

(defun log-info (format &rest args)
  (log-message
   (make-instance 'message :level :info
                           :text (apply #'format nil format args))))

(defun log-info-data (data format &rest args)
  (log-message
   (make-instance 'message :level :info
                           :text (apply #'format nil format args)
                           :data data)))

(defun log-error (format &rest args)
  (log-message
   (make-instance 'message :level :error
                           :text (apply #'format nil format args))))

(defun log-error-data (data format &rest args)
  (log-message
   (make-instance 'message :level :error
                           :text (apply #'format nil format args)
                           :data data)))
