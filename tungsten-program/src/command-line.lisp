(in-package :program)

(define-condition command-line-error (error)
  ())

(define-condition invalid-option-format (command-line-error)
  ((argument
    :type string
    :initarg :argument
    :reader invalid-option-format-argument))
  (:report
   (lambda (condition stream)
     (format stream "invalid option format ~S"
             (invalid-option-format-argument condition)))))

(define-condition unknown-option (command-line-error)
  ((name
    :type string
    :initarg :name
    :reader unknown-option-name))
  (:report
   (lambda (condition stream)
     (let ((name (unknown-option-name condition)))
       (format stream "unknown option ~S"
               (concatenate
                'string (if (= (length name) 1) "-" "--") name))))))

(define-condition missing-option-value (command-line-error)
  ((name
    :type string
    :initarg :name
    :reader missing-option-value-name))
  (:report
   (lambda (condition stream)
     (let ((name (missing-option-value-name condition)))
       (format stream "missing value for option ~S"
               (concatenate
                'string (if (= (length name) 1) "-" "--") name))))))

(define-condition missing-arguments (command-line-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "missing argument(s)"))))

(define-condition too-many-arguments (command-line-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "too many arguments"))))

(define-condition missing-command (command-line-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "missing command" stream))))

(define-condition unknown-command (command-line-error)
  ((name
    :type string
    :initarg :name
    :reader unknown-command-name))
  (:report
   (lambda (condition stream)
     (let ((name (unknown-command-name condition)))
       (format stream "unknown command ~S" name)))))

(define-condition usage-printed (condition)
  ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  #-(or sbcl ccl)
  (core:unsupported-feature "command line handling"))

(defun argv ()
  #+sbcl sb-ext:*posix-argv*
  #+ccl  ccl:*command-line-argument-list*)

(defun command-line-program-name ()
  (car (argv)))

(defun command-line-arguments ()
  (cdr (argv)))

(defclass command ()
  ((name
    :type (or string null)
    :initarg :name
    :initform nil
    :reader command-name)
   (description
    :type (or string null)
    :initarg :description
    :reader command-description)
   (options
    :type hash-table
    :initform (make-hash-table :test #'equal)
    :reader command-options)
   (arguments
    :type list
    :initform nil
    :accessor command-arguments)
   (function
    :type (or symbol function)
    :initarg :function
    :reader command-function)))

(defmethod initialize-instance :after ((command command)
                                       &key &allow-other-keys)
  (command-add-option
   command (make-instance 'option :short-name "h"
                                  :long-name "help"
                                  :description "print help and exit")))

(defmethod print-object ((command command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream "~A" (command-name command))))

(defclass option ()
  ((short-name
    :type (or string null)
    :initarg :short-name
    :initform nil
    :reader option-short-name)
   (long-name
    :type (or string null)
    :initarg :long-name
    :initform nil
    :reader option-short-name)
   (value-name
    :type (or string null)
    :initarg :value-name
    :initform nil
    :reader option-value-name)
   (default-value
    :type (or string null)
    :initarg :default-value
    :initform nil
    :reader option-default-value)
   (description
    :type string
    :initarg :description
    :reader option-description)))

(defmethod initialize-instance :after ((option option) &key &allow-other-keys)
  (with-slots (short-name long-name) option
    (unless (or short-name long-name)
      (error "option must have a name"))))

(defmethod print-object ((option option) stream)
  (print-unreadable-object (option stream :type t)
    (with-slots (short-name long-name) option
      (when short-name
        (format stream "-~A" short-name))
      (when long-name
        (when short-name
          (write-char #\Space stream))
        (format stream "--~A" long-name)))))

(defclass argument ()
  ((name
    :type string
    :initarg :name
    :reader argument-name)
   (description
    :type string
    :initarg :description
    :reader argument-description)
   (trailing
    :type boolean
    :initarg :trailing
    :reader argument-trailing)))

(defmethod print-object ((argument argument) stream)
  (print-unreadable-object (argument stream :type t)
    (format stream "~A" (argument-name argument))))

(defun command-add-option (command option)
  (declare (type command command)
           (type option option))
  (with-slots (options) command
    (flet ((register (slot)
             (let ((name (slot-value option slot)))
               (when name
                 (when (gethash name options)
                   (error "duplicate option ~S" (string name)))
                 (setf (gethash name options) option)))))
      (register 'short-name)
      (register 'long-name))))

(defun parse-command-line (commands program-name arguments)
  (declare (type hash-table commands)
           (type string program-name)
           (type list arguments))
  (let ((option-values (make-hash-table :test #'equal))
        (argument-values (make-hash-table :test #'equal))
        (has-subcommands (> (hash-table-count commands) 1))
        (command (gethash nil commands)))
    (labels ((set-option-value (option value)
               (with-slots (short-name long-name) option
                 (when short-name
                   (setf (gethash short-name option-values) value))
                 (when long-name
                   (setf (gethash long-name option-values) value))))
             (parse-options ()
               (do ((argument (car arguments) (car arguments)))
                   ((null arguments)
                    nil)
                 (cond
                   ((string= argument "--")
                    (pop arguments)
                    (return))
                   ((and (> (length argument) 0)
                         (char= (char argument 0) #\-))
                    (when (and (> (length argument) 2)
                               (char/= (char argument 1) #\-))
                      (error 'invalid-option-format :argument argument))
                    (let* ((longp (and (> (length argument) 1)
                                       (char= (char argument 1) #\-)))
                           (name (subseq argument (if longp 2 1)))
                           (option (or (gethash name (command-options command))
                                       (error 'unknown-option :name name))))
                      (pop arguments)
                      (cond
                        ((option-value-name option)
                         (when (null arguments)
                           (error 'missing-option-value :name name))
                         (set-option-value option (pop arguments)))
                        (t
                         (set-option-value option t)))))
                   (t
                    (return)))))
             (parse-arguments ()
               (do ((argument (car arguments) (car arguments))
                    (program-arguments (command-arguments command)))
                   ((null arguments)
                    (when program-arguments
                      (error 'missing-arguments)))
                 (let ((program-argument (or (car program-arguments)
                                             (error 'too-many-arguments))))
                   (cond
                     ((argument-trailing program-argument)
                      (setf (gethash (argument-name program-argument)
                                     argument-values)
                            arguments)
                      (setf arguments nil)
                      (pop program-arguments))
                     (t
                      (setf (gethash (argument-name program-argument)
                                     argument-values)
                            argument)
                      (pop arguments)
                      (pop program-arguments))))))
             (maybe-print-usage ()
               (when (gethash "help" option-values)
                 (print-usage commands command program-name *standard-output*)
                 (signal 'usage-printed))))
      (parse-options)
      (cond
        (has-subcommands
         (when (zerop (length arguments))
           (error 'missing-command))
         (let ((name (pop arguments)))
           (setf command (or (gethash name commands)
                             (error 'unknown-command :name name))))
         (parse-options)
         (maybe-print-usage)
         (parse-arguments))
        (t
         (maybe-print-usage)
         (parse-arguments))))
    (values command option-values argument-values)))

(defun print-usage (commands command program-name stream)
  (declare (type hash-table commands)
           (type command command)
           (type string program-name)
           (type stream stream))
  (let* ((top-level-command-p (null (command-name command)))
         (top-level-command (gethash nil commands))
         (has-subcommands (> (hash-table-count commands) 1))
         (arguments (command-arguments (if top-level-command-p
                                           top-level-command
                                           command)))
         (max-width (usage-first-column-width commands command)))
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

(defun usage-first-column-width (commands command)
  (declare (type hash-table commands)
           (type command command))
  (let* ((command-list (core:hash-table-values commands))
         (top-level-command-p (null (command-name command)))
         (top-level-command (gethash nil commands))
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
      (let ((lengths (append (mapcar #'command-length command-list)
                             (mapcar #'argument-length arguments)
                             (mapcar #'option-length options))))
        (reduce #'max lengths)))))
