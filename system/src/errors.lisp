(in-package :system)

(define-condition system-error (error)
  ((function
    :type string
    :initarg :function
    :reader system-error-function)
   (value
    :type (or keyword integer)
    :initarg :value
    :reader system-error-value)
   (description
    :type (or string null)
    :initarg :description
    :initform nil
    :reader system-error-description))
  (:report
   (lambda (c stream)
     (with-slots (function value description) c
       (format stream "System function ~S failed~@[ with error ~A~]~@[: ~A~]."
               function value description)))))

(defun errno-value (return-value)
  (declare (ignore return-value))
  (ffi:errno))

;; POSIX defines three functions that return a description of an errno value:
;; strerror(), strerror_r(), and strerror_l(). Infortunately strerror() is not
;; thread safe, and strerror_r() has two versions (GNU and POSIX) that cannot
;; be differenciated at runtime (http://austingroupbugs.net/view.php?id=655).
;; Therefore we only use strerror_l().
(defun strerror-l (errno)
  (let ((locale (ffi:foreign-funcall "uselocale" ((locale-t) locale-t) 0)))
    (when (zerop locale)
      (error 'system-error :function "uselocale" :value (ffi:errno)))
    (let ((ptr (ffi:foreign-funcall
                "strerror_l" ((:int locale-t) :pointer) errno locale)))
      (when (ffi:null-pointer-p ptr)
        (error 'system-error :function "strerror_l" :value (ffi:errno)))
      (ffi:decode-foreign-string ptr))))

(defmacro system-funcall ((name signature &rest args)
                          &key (errorp
                                (case (car (last signature))
                                  (:void 'nil)
                                  (:pointer ''ffi:null-pointer-p)
                                  (t ''minus1p)))
                               (error-value-function ''errno-value)
                               (error-description-function ''strerror-l)
                               (retryp ''eintrp))
  "Call a foreign function.

If ERRORP is not null, it is used to determine if the return value of the call
indicates an error. When this is the case, signal a SYSTEM-ERROR condition
containing information about the error.

If ERROR-VALUE-FUNCTION is not null, it is used to obtain a value representing
the error.

If ERROR-DESCRIPTION-FUNCTION is not null, it is used to obtain a textual
description of the error value.

If RETRYP is not null, use it to determine if the value of errno indicates
that the call should be executed again. If this is the case, execute the call
again with the same arguments."
  (let ((errorp-var (gensym "ERRORP-VAR-"))
        (retryp-var (gensym "RETRYP-VAR-"))
        (value (gensym "VALUE-"))
        (error-value (gensym "ERROR-VALUE-")))
    `(let ((,errorp-var ,errorp)
           (,retryp-var ,retryp))
       (block body
         (tagbody retry
            (let ((,value (ffi:foreign-funcall ,name ,signature ,@args)))
              (when (and ,errorp-var (funcall ,errorp-var ,value))
                (let ((,error-value (funcall ,error-value-function ,value)))
                  (if (and ,retryp-var (funcall ,retryp-var ,error-value))
                      (go retry)
                      (error 'system-error
                             :function ,name
                             :value ,error-value
                             :description (funcall ,error-description-function
                                                   ,error-value)))))
              (return-from body ,value)))))))

(declaim (inline minus1p))
(defun minus1p (n)
  (declare (integer n))
  (= n -1))

(declaim (inline eintrp))
(defun eintrp (value)
  (declare (type integer value))
  (= value #.(ffi:encode-foreign-value :eintr 'errno)))
