(in-package :sqlite)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'sqlite "libsqlite3.so")

(defun library-version ()
  (let ((%string (ffi:foreign-funcall "sqlite3_libversion" (() :pointer))))
    (ffi:decode-foreign-string %string)))

;;;
;;; Errors
;;;

(define-condition sqlite-error (error)
  ((function
    :type string
    :initarg :function
    :reader sqlite-error-function)
   (code
    :type integer
    :initarg :code
    :reader sqlite-error-code)
   (description
    :type string
    :initarg :description
    :reader sqlite-error-description))
  (:report
   (lambda (condition stream)
     (with-slots (function code description) condition
       (format stream "SQLite function ~S failed with code ~A: ~A."
               function code description)))))

(defmacro sqlite-funcall ((name signature &rest args))
  (let ((code (gensym "CODE-"))
        (description (gensym "DESCRIPTION-")))
    `(let ((,code (ffi:foreign-funcall ,name ,signature ,@args)))
       (unless (zerop ,code)
         (let ((,description
                 (ffi:decode-foreign-string
                  (ffi:foreign-funcall "sqlite3_errstr"
                                       ((:int) :pointer) ,code))))
           (error 'sqlite-error :function ,name
                                :code ,code
                                :description ,description))))))
