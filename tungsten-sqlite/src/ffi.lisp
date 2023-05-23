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
    :type (or symbol integer)
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

(defun sqlite-error (function-name code)
  (let ((description
          (ffi:decode-foreign-string
           (ffi:foreign-funcall "sqlite3_errstr"
                                ((:int) :pointer) code))))
    (error 'sqlite-error
           :function function-name
           :code (ffi:decode-foreign-value code 'result-code)
           :description description)))

(defmacro sqlite-funcall ((function-name signature &rest args))
  (let ((code (gensym "CODE-")))
    `(let ((,code (ffi:foreign-funcall ,function-name ,signature ,@args)))
       (unless (zerop ,code)
         (sqlite-error ,function-name ,code)))))

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

;;;
;;; Statements
;;;

(defun sqlite3-prepare-v3 (%db query flags)
  (ffi:with-foreign-string (%query query)
    (ffi:with-foreign-values ((%stmt :pointer)
                              (%tail :pointer))
      (sqlite-funcall
       ("sqlite3_prepare_v3"
        ((:pointer :pointer :int prepare-flags :pointer :pointer) :int)
        %db %query -1 flags %stmt %tail))
      (values (ffi:foreign-value %stmt :pointer)
              (ffi:decode-foreign-string
               (ffi:foreign-value %tail :pointer))))))

(defun sqlite3-finalize (%stmt)
  (sqlite-funcall ("sqlite3_finalize" ((:pointer) :int) %stmt)))

(defun sqlite3-bind-blob64 (%stmt n value)
  (ffi:with-pinned-vector-data (%value value)
    (sqlite-funcall
     ("sqlite3_bind_blob64"
      ((:pointer :int :pointer uint64 destructor-type) :int)
      %stmt n %value (length value) :transient))))

(defun sqlite3-bind-double (%stmt n value)
  (sqlite-funcall
   ("sqlite3_bind_double" ((:pointer :int :double) :int) %stmt n value)))

(defun sqlite3-bind-int64 (%stmt n value)
  (sqlite-funcall
   ("sqlite3_bind_int64" ((:pointer :int int64) :int) %stmt n value)))

(defun sqlite3-bind-null (%stmt n)
  (sqlite-funcall
   ("sqlite3_bind_null" ((:pointer :int) :int) %stmt n)))

(defun sqlite3-bind-text (%stmt n value)
  (ffi:with-foreign-string (%value value)
    (sqlite-funcall
     ("sqlite3_bind_text"
      ((:pointer :int :pointer :int destructor-type) :int)
      %stmt n %value -1 :transient))))

(defun sqlite3-step (%stmt)
  (let ((code (ffi:foreign-funcall
               "sqlite3_step" ((:pointer) result-code) %stmt)))
    (case code
      (:done
       (return-from sqlite3-step nil))
      (:row
       (return-from sqlite3-step t))
      (t
       (sqlite-error "sqlite3_step" code)))))

(defun sqlite3-data-count (%stmt)
  (ffi:foreign-funcall "sqlite3_data_count" ((:pointer) :int) %stmt))

(defun sqlite3-column-type (%stmt index)
  (ffi:foreign-funcall "sqlite3_column_type"
                       ((:pointer :int) data-type) %stmt index))

(defun sqlite3-column-bytes (%stmt index)
  (ffi:foreign-funcall "sqlite3_column_bytes"
                       ((:pointer :int) :int) %stmt index))

(defun sqlite3-column-blob (%stmt index)
  (let ((%data (ffi:foreign-funcall "sqlite3_column_blob"
                                    ((:pointer :int) :pointer) %stmt index))
        (length (sqlite3-column-bytes %stmt index)))
    (ffi:read-foreign-memory %data length)))

(defun sqlite3-column-double (%stmt index)
  (ffi:foreign-funcall "sqlite3_column_double"
                       ((:pointer :int) :double) %stmt index))

(defun sqlite3-column-int64 (%stmt index)
  (ffi:foreign-funcall "sqlite3_column_int64"
                       ((:pointer :int) int64) %stmt index))

(defun sqlite3-column-text (%stmt index)
  (let ((%string (ffi:foreign-funcall "sqlite3_column_text"
                                      ((:pointer :int) :pointer) %stmt index))
        (length (sqlite3-column-bytes %stmt index)))
    (ffi:decode-foreign-string %string :length length)))
