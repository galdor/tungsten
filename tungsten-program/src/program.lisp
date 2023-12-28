(in-package :program)

(defvar *programs* (make-hash-table))

(defvar *program* nil)
(defvar *command* nil)

(defclass program ()
  ((name
    :type symbol
    :initarg :name
    :reader program-name)
   (commands
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :reader program-commands)
   (option-values
    :type hash-table
    :accessor program-option-values)
   (argument-values
    :type hash-table
    :accessor program-argument-values)))

(define-condition unknown-program (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-program-name))
  (:report
   (lambda (condition stream)
     (format stream "unknown program ~A" (unknown-program-name condition)))))

(defmethod print-object ((program program) stream)
  (print-unreadable-object (program stream :type t)
    (format stream "~A" (program-name program))))

(defmacro defprogram ((name) &key commands options arguments function)
  (let ((program (gensym "PROGRAM-")))
    `(let* ((,program (make-instance 'program :name ',name)))
       (let ((*program* ,program))
         (register-command nil :options ',options
                               :arguments ',arguments
                               :function ,function)
         ,@(mapcar (lambda (command)
                     (destructuring-bind (name &key options arguments function)
                         command
                       `(register-command ,name :options ',options
                                                :arguments ',arguments
                                                :function ,function)))
                   commands))
       (setf (gethash ',name *programs*) ,program))))

(defun register-command (name &key options arguments function)
  (declare (type (or string null) name)
           (type list options arguments)
           (type (or symbol function) function))
  (when name
    (let ((top-level-command (gethash nil (program-commands *program*))))
      (when (command-arguments top-level-command)
        (error "cannot use commands with top-level command arguments"))
      (when (command-function top-level-command)
        (error "cannot use commands with a top-level command function"))))
  (let ((command (make-instance 'command :name name :function function)))
    (let ((*command* command))
      (dolist (option options)
        (register-option option))
      (dolist (argument arguments)
        (register-argument argument)))
    (setf (gethash name (program-commands *program*)) command)))

(defun register-option (option-data)
  (declare (type list option-data))
  (destructuring-bind (short-name long-name value-name description
                       &key default-value)
      option-data
    (let ((option (make-instance 'option
                                 :short-name short-name
                                 :long-name long-name
                                 :value-name value-name
                                 :description description
                                 :default-value (or default-value ""))))
      (flet ((register (slot)
               (let ((name (slot-value option slot)))
                 (when name
                   (with-slots (options) *command*
                     (when (gethash name options)
                       (error "duplicate option ~S" (string name)))
                     (setf (gethash name options) option))))))
        (register 'short-name)
        (register 'long-name)))))

(defun register-argument (argument-data)
  (declare (type list argument-data))
  (destructuring-bind (name description &key trailing) argument-data
    (let ((last-argument (car (command-arguments *command*))))
      (when (and last-argument (argument-trailing last-argument))
        (error "cannot add arguments after a trailing argument")))
    (let ((argument (make-instance 'argument :name name
                                             :description description
                                             :trailing trailing)))
      (push argument (command-arguments *command*)))))

(defun program (program)
  (declare (type (or symbol program) program))
  (etypecase program
    (symbol
     (or (gethash program *programs*)
         (error 'unknown-program :name program)))
    (program
     program)))

(defun build-executable (program-name &key path compression)
  (declare (type symbol program-name)
           (type (or pathname string null) path)
           (type boolean compression))
  (let ((path (or path (string-downcase (symbol-name program-name))))
        (program (program program-name)))
    #+sbcl
    (let ((args (append
                 (list :executable t)
                 (list :toplevel (top-level-function program))
                 (when (and (member :sb-core-compression *features*)
                            compression)
                   (list :compression t)))))
      (apply 'sb-ext:save-lisp-and-die path args))
    #+ccl
    (ccl:save-application path :prepend-kernel t
                               :purify t
                               :toplevel-function (top-level-function program))
    #-(or sbcl ccl)
    (core:unsupported-feature "executable creation")))

(defun add-option (short-name long-name value-name description
                   &key default-value)
  (declare (type (or string null) short-name)
           (type (or string null) long-name default-value)
           (type string value-name description))
  (let ((option (make-instance 'option :short-name short-name
                                       :long-name long-name
                                       :value-name value-name
                                       :default-value (or default-value "")
                                       :description description)))
    (flet ((register (name)
             (declare (type (or string string) name))
             (with-slots (options) *command*
               (when (gethash name options)
                 (error "duplicate option ~S" (string name)))
               (setf (gethash name options) option))))
      (when short-name
        (register short-name))
      (when long-name
        (register long-name)))))

(defun add-argument (name description &key trailing)
  (declare (type string name description)
           (type boolean trailing))
  (let ((argument (make-instance 'argument :name name
                                           :description description
                                           :trailing trailing)))
    (push argument (command-arguments *command*))))

(defun top-level-function (program)
  (declare (type program program))
  (lambda ()
    (handler-case
        (let ((core:*interactive* nil))
          (run-program program
                       (command-line-program-name) (command-line-arguments)))
      ((or unknown-option missing-option-value
           missing-arguments too-many-arguments)
          (condition)
        (format *error-output* "error: ~A~%" condition))
      (condition (condition)
        ;; Yes, conditions are not necessarily errors, but an unhandled
        ;; condition at top-level is definitely an error.
        (format *error-output* "error: ~A~%~%" condition)
        (core:format-backtrace (core:backtrace :start 1) *error-output*
                               :include-source-file t)))))

(defun run-program (program program-name program-arguments)
  (declare (type (or symbol program) program)
           (type string program-name)
           (type list program-arguments))
  (let ((program (program program)))
    (multiple-value-bind (command option-values argument-values)
        (parse-command-line (program-commands program)
                            program-name program-arguments)
      (setf (program-option-values program) option-values
            (program-argument-values program) argument-values)
      (let ((*program* program)
            (*command* command))
        (funcall (command-function *command*))))))
