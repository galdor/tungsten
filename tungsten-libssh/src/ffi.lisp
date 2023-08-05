(in-package :libssh)

;;;
;;; Library
;;;

(ffi:use-foreign-library 'ssh "libssh.so.4")

(defun library-version ()
  "Return a string containing the version number of the libssh library."
  ;; The version string is formatted as:
  ;;
  ;; version-string = version { "/" feature }
  (let* ((string (ffi:decode-foreign-string
                  (ffi:foreign-funcall "ssh_version" ((:int) :pointer) 0)))
         (version-end (or (position #\/ string) (length string))))
    (subseq string 0 version-end)))

;;;
;;; Errors
;;;

(define-condition libssh-error (error)
  ((function
    :type string
    :initarg :function
    :reader libssh-error-function)
   (code
    :type (or keyword integer null)
    :initarg :code
    :initform nil
    :reader libssh-error-code)
   (description
    :type string
    :initarg :description
    :initform nil
    :reader libssh-error-description))
  (:report
   (lambda (c stream)
     (with-slots (function code description) c
       (format stream "Libssh function ~S failed~@[ with error ~A~]: ~A."
               function code description)))))

(defun libssh-error (function-name error-source)
  (declare (type string function-name)
           (type (or ffi:pointer null) error-source))
  (let (code description)
    (when error-source
      (setf code
            (ffi:foreign-funcall "ssh_get_error_code"
                                 ((:pointer) :int) error-source))
      (setf description
            (ffi:decode-foreign-string
             (ffi:foreign-funcall "ssh_get_error"
                                  ((:pointer) :pointer) error-source))))
    (error 'libssh-error :function function-name
                         :code code
                         :description (or description "unknown error"))))

(defmacro libssh-funcall ((function-name signature &rest args)
                          &key error-source)
  (let ((result (gensym "RESULT-"))
        (result-type (second signature)))
    `(let ((,result (ffi:foreign-funcall ,function-name ,signature ,@args)))
       (when ,(case result-type
                (:pointer
                 `(ffi:null-pointer-p ,result))
                (:int
                 `(< ,result 0))
                (ssh-error
                 `(not (eql ,result :ssh-ok)))
                (ssh-known-hosts-status
                 `(eql ,result :ssh-known-hosts-error)))
         (libssh-error ,function-name ,error-source))
       ,result)))

;;;
;;; Sessions
;;;

(defun ssh-new ()
  (libssh-funcall ("ssh_new" (() :pointer))))

(defun ssh-free (%session)
  (libssh-funcall ("ssh_free" ((:pointer) :void) %session)))

(defun ssh-connect (%session)
  (libssh-funcall ("ssh_connect" ((:pointer) ssh-error) %session)
                  :error-source %session))

(defun ssh-disconnect (%session)
  (libssh-funcall ("ssh_disconnect" ((:pointer) :void) %session)
                  :error-source %session))

(defun ssh-options-set (%session option %value)
  (declare (type ffi:pointer %session %value)
           (type keyword option))
  (libssh-funcall ("ssh_options_set" ((:pointer ssh-option :pointer) :int)
                                     %session option %value)
                  :error-source %session))

(defun ssh-options-set/string (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type string value))
  (ffi:with-foreign-string (%value value)
    (ssh-options-set %session option %value)))

(defun ssh-options-set/int (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type integer value))
  (ffi:with-foreign-value (%value :int)
    (setf (ffi:foreign-value %value :int) value)
    (ssh-options-set %session option %value)))

(defun ssh-options-set/unsigned-int (%session option value)
  (declare (type ffi:pointer %session)
           (type keyword option)
           (type (integer 0) value))
  (ffi:with-foreign-value (%value :unsigned-int)
    (setf (ffi:foreign-value %value :unsigned-int) value)
    (ssh-options-set %session option %value)))

(defun ssh-get-server-publickey (%session)
  (declare (type ffi:pointer %session))
  (ffi:with-foreign-value (%key :pointer)
    (libssh-funcall ("ssh_get_server_publickey" ((:pointer :pointer) :int)
                                                %session %key)
                    :error-source %session)
    (ffi:foreign-value %key :pointer)))

(defun ssh-session-is-known-server (%session)
  (declare (type ffi:pointer %session))
  (libssh-funcall ("ssh_session_is_known_server"
                   ((:pointer) ssh-known-hosts-status) %session)
                  :error-source %session))

;;;
;;; Keys
;;;

(defun ssh-key-free (%key)
  (declare (type ffi:pointer %key))
  (libssh-funcall ("ssh_key_free" ((:pointer) :void) %key)))

(defun ssh-get-publickey-hash (%key hash-type)
  (declare (type ffi:pointer %key)
           (type keyword hash-type))
  (ffi:with-foreign-values ((%hash :pointer)
                            (%hash-size 'system:size-t))
    (libssh-funcall ("ssh_get_publickey_hash"
                     ((:pointer ssh-publickey-hash :pointer :pointer) :int)
                     %key hash-type %hash %hash-size))
    (unwind-protect
         (let ((hash-size (ffi:foreign-value %hash-size 'system:size-t)))
           (ffi:read-foreign-memory (ffi:foreign-value %hash :pointer)
                                    hash-size))
      (libssh-funcall ("ssh_clean_pubkey_hash" ((:pointer) :void) %hash)))))
