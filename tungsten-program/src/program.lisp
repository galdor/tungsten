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

(defun program-command (program name)
  (declare (type program program)
           (type (or string null) name))
  (gethash name (program-commands program)))

(defun program-top-level-command (program)
  (declare (type program program))
  (program-command program nil))

(defmacro defprogram ((name) &key commands options arguments function)
  (let ((program (gensym "PROGRAM-")))
    `(let* ((,program (make-instance 'program :name ',name)))
       (let ((*program* ,program))
         (register-command nil nil :options ',options
                                   :arguments ',arguments
                                   :function ,function)
         ,@(mapcar
            (lambda (command)
              (destructuring-bind (name description
                                   &key options arguments function)
                  command
                `(register-command ,name ,description
                                   :options ',options
                                   :arguments ',arguments
                                   :function ,function)))
                   commands))
       (setf (gethash ',name *programs*) ,program))))

(defun register-command (name description &key options arguments function)
  (declare (type (or string null) name description)
           (type list options arguments)
           (type (or symbol function) function))
  (when name
    (let ((top-level-command (program-top-level-command *program*)))
      (when (command-arguments top-level-command)
        (error "cannot use commands with top-level command arguments"))
      (when (command-function top-level-command)
        (error "cannot use commands with a top-level command function"))))
  (let ((command (make-instance 'command :name name
                                         :description description
                                         :function function)))
    (let ((*command* command))
      (dolist (option options)
        (register-option option))
      (dolist (argument arguments)
        (register-argument argument))
      (setf (command-arguments command)
            (nreverse (command-arguments command))))
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
                                 :default-value default-value
                                 :description description)))
      (command-add-option *command* option))))

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
    ;; Note that for SBCL, we use :SAVE-RUNTIME-OPTIONS to stop SBCL from
    ;; processing command line arguments on its own at startup. Without it,
    ;; --help is being intercepted by the SBCL runtime before entering into
    ;; our top-level function.
    #+sbcl
    (let ((args (append
                 (list :executable t)
                 (list :save-runtime-options t)
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

(defun top-level-function (program)
  (declare (type program program))
  (lambda ()
    (handler-case
        (let ((core:*interactive* nil))
          (run-program program
                       (command-line-program-name) (command-line-arguments)))
      (command-line-error (condition)
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
    (handler-bind
        ((usage-printed
           (lambda (condition)
             (declare (ignore condition))
             (return-from run-program nil))))
      (multiple-value-bind (command option-values argument-values)
          (parse-command-line (program-commands program)
                              program-name program-arguments)
        (setf (program-option-values program) option-values
              (program-argument-values program) argument-values)
        (let ((*program* program)
              (*command* command))
          (funcall (command-function *command*)))))))

(defun option (name)
  (declare (type string name))
  (or (gethash name (command-options *command*))
      (error 'unknown-option :name name)))

(defun option-set-p (name)
  (declare (type string name))
  (gethash name (program-option-values *program*)))

(defun option-value (name)
  (declare (type string name))
  (let ((option (option name)))
    (or (gethash name (program-option-values *program*))
        (option-default-value option))))

(defun argument-value (name)
  (declare (type string name))
  (gethash name (program-argument-values *program*)))
