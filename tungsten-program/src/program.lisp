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

(defun option (name)
  (declare (type string name))
  (or (gethash name (command-options *command*))
      (error 'unknown-option :name name)))

(defun option-value (name)
  (declare (type string name))
  (let ((option (option name)))
    (or (gethash name (program-option-values *program*))
        (option-default-value option))))

(defun argument-value (name)
  (declare (type string name))
  (gethash name (program-argument-values *program*)))

(defun print-usage (program program-name command stream)
  (declare (type program program)
           (type string program-name)
           (type command command)
           (type stream stream))
  (let* ((commands (program-commands program))
         (top-level-command-p (null (command-name command)))
         (top-level-command (program-top-level-command program))
         (has-subcommands (> (hash-table-count commands) 1))
         (arguments (command-arguments (if top-level-command-p
                                           top-level-command
                                           command)))
         (max-width (usage-first-column-width program command)))
    ;; Usage line
    (cond
      ((and top-level-command-p has-subcommands)
       (format stream "Usage: ~A OPTIONS <command>~%" program-name))
      ((> (length arguments) 0)
       (format stream "Usage: ~A~@[ ~A~] OPTIONS"
               program-name (command-name command))
       (dolist (argument arguments)
         (cond
           ((argument-trailing argument)
            (format stream " <~A>..." (argument-name argument)))
           (t
            (format stream " <~A>" (argument-name argument)))))
       (terpri stream))
      (t
       (format stream "Usage: ~A OPTIONS~%" program-name)))
    ;; Commands or arguments
    (cond
      ((and top-level-command-p has-subcommands)
       (format stream "~%COMMANDS~%~%")
       (let ((names (sort (delete nil (core:hash-table-keys commands))
                          #'string<=)))
         (dolist (name names)
           (let ((command (gethash name commands)))
             (format stream "~vA  ~A~%"
                     max-width name (command-description command))))))
      ((> (length arguments) 0)
       (format stream "~%ARGUMENTS~%~%")
       (dolist (argument (command-arguments command))
         (format stream "~vA  ~A~%"
                 max-width (argument-name argument)
                 (argument-description argument)))))
    ;; Options
    (labels ((format-option-string (option)
               (declare (type option option))
               (with-slots (short-name long-name value-name default-value
                            description)
                   option
                 (format nil "~:[~*    ~;-~A~]~@[~*, ~]~@[--~A~]~
                              ~@[ <~A>~]"
                         short-name short-name
                         (and short-name long-name) long-name value-name)))
             (format-option (option)
               (declare (type option option))
               (with-slots (description default-value) option
                 (format stream "~vA  ~A~@[ (default: ~S)~]~%" max-width
                         (format-option-string option) description
                         default-value)))
             (format-options (label options)
               (declare (type string label)
                        (type hash-table options))
               (format stream "~%~A~%~%" label)
               (let ((names (sort (core:hash-table-keys options) #'string<=))
                     (printed-options nil))
                 (dolist (name names)
                   (let ((option (gethash name options)))
                     (unless (member option printed-options)
                       (format-option option)
                       (push option printed-options)))))))
      (when (> (hash-table-count (command-options top-level-command)) 0)
        (format-options (if has-subcommands
                            "GLOBAL OPTIONS"
                            "OPTIONS")
                        (command-options top-level-command)))
      (when (and (not top-level-command-p)
                 (> (hash-table-count (command-options command)) 0))
        (format-options "COMMAND OPTIONS" (command-options command))))))

(defun usage-first-column-width (program command)
  (declare (type program program)
           (type command command))
  (let* ((commands (core:hash-table-values (program-commands program)))
         (top-level-command-p (null (command-name command)))
         (top-level-command (program-top-level-command program))
         (arguments (command-arguments (if top-level-command-p
                                           top-level-command
                                           command)))
         (options (append
                   (core:hash-table-values (command-options top-level-command))
                   (unless top-level-command-p
                     (core:hash-table-values (command-options command))))))
    (flet ((command-length (command)
             (with-slots (name) command
               (if name (length name) 0)))
           (argument-length (argument)
             (length (argument-name argument)))
           (option-length (option)
             (with-slots (long-name value-name) option
               (+ 2                     ; short option or padding
                  2                     ; ", "
                  (if long-name
                      (+ 2              ; "--"
                         (length long-name))
                      0)
                  (if value-name
                      (+ 2              ; " <"
                         (length value-name)
                         1)             ; ">"
                      0)))))
      (let ((lengths (append (mapcar #'command-length commands)
                             (mapcar #'argument-length arguments)
                             (mapcar #'option-length options))))
        (reduce #'max lengths)))))
