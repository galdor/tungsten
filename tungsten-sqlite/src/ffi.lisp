(in-package :sqlite)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'sqlite "libsqlite3.so")

(defun sqlite3-libversion ()
  (let ((%string (ffi:foreign-funcall "sqlite3_libversion" (() :pointer))))
    (ffi:decode-foreign-string %string)))

(defun sqlite3-threadsafe ()
  (ffi:foreign-funcall "sqlite3_threadsafe" (() :int)))

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

;;;
;;; Databases
;;;

(defun sqlite3-open-v2 (filename flags vfs-module-name)
  (ffi:with-foreign-strings ((%filename filename)
                             (%vfs-module-name vfs-module-name))
    (ffi:with-foreign-value (%database :pointer)
      (sqlite-funcall ("sqlite3_open_v2"
                       ((:pointer :pointer open-flags :pointer) :int)
                       %filename %database flags %vfs-module-name))
      (ffi:foreign-value %database :pointer))))

(defun sqlite3-close-v2 (%db)
  (sqlite-funcall ("sqlite3_close_v2" ((:pointer) :int) %db)))

(defun sqlite3-extended-result-codes (%db enable)
  (sqlite-funcall
   ("sqlite3_extended_result_codes" ((:pointer :int) :int)
                                    %db (if enable 1 0))))
