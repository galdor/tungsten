(in-package :core)

(defparameter *backtrace-depth* 20)
(defparameter *include-backtrace-source-files* nil)

(defclass frame ()
  ((number
    :type (integer 0)
    :initarg :number
    :reader frame-number)
   (name
    :type (or string symbol list)
    :initarg :name
    :reader frame-name)
   (source-file
    :type (or pathname null)
    :initarg :source-file
    :initform nil
    :reader frame-source-file)))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :type t)
    (format stream "~S" (frame-name frame))))

(defun format-frame (frame stream
                     &key (include-source-file
                           *include-backtrace-source-files*))
  (declare (type frame frame)
           (type stream stream)
           (type boolean include-source-file))
  (with-slots (number name source-file) frame
    (format stream "~2D ~S~%" number name)
    (when (and source-file include-source-file)
      (format stream "   ~A~%" source-file))))

(defun format-backtrace (backtrace stream
                         &key (include-source-file
                               *include-backtrace-source-files*))
    (declare (type list backtrace)
             (type stream stream)
             (type boolean include-source-file))
  (dolist (frame backtrace)
    (format-frame frame stream :include-source-file include-source-file)))

#+ccl
(progn
  (declaim (inline %backtrace))
  (defun %backtrace (start count)
    (let ((frames nil)
          (number 0))
      (ccl:map-call-frames
       (lambda (pointer context)
         (declare (ignore context))
         (let* ((function (ccl::cfp-lfun pointer))
                (name (ccl::lfun-name function))
                (source (ccl:function-source-note function))
                (source-file (let ((string (ccl:source-note-filename source)))
                               (when string
                                 (pathname string)))))
           (push (make-instance 'frame :number number
                                       :name name
                                       :source-file source-file)
                 frames)
           (incf number)))
       :start-frame-number start :count count)
      (nreverse frames))))

#+sbcl
(progn
  (declaim (inline %backtrace))
  (defun %backtrace (start count)
    (let ((frames nil))
      (sb-debug:map-backtrace
       (lambda (frame)
         (let* ((number (sb-di:frame-number frame))
                (name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))
                (location (sb-di:frame-code-location frame))
                (source (sb-di:code-location-debug-source location))
                (source-file
                  (let ((string (sb-di:debug-source-namestring source)))
                    (when string
                      (pathname string)))))
           (push (make-instance 'frame :number number
                                       :name name
                                       :source-file source-file)
                 frames)))
       :from :current-frame :start start :count count)
      (nreverse frames))))

(defun backtrace (&key (start 0) (depth *backtrace-depth*))
  (declare (type (integer 0) start depth))
  (%backtrace start depth))
