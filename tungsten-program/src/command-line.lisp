(in-package :program)

(define-condition invalid-option-format (error)
  ((argument
    :type string
    :initarg :argument
    :reader invalid-option-format-argument))
  (:report
   (lambda (condition stream)
     (format stream "invalid option format ~S"
             (invalid-option-format-argument condition)))))

(define-condition unknown-option (error)
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

(define-condition missing-option-value (error)
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

(define-condition missing-arguments (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "missing argument(s)"))))

(define-condition too-many-arguments (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "too many arguments"))))

(define-condition missing-command (error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "missing command" stream))))

(define-condition unknown-command (error)
  ((name
    :type string
    :initarg :name
    :reader unknown-command-name))
  (:report
   (lambda (condition stream)
     (let ((name (unknown-command-name condition)))
       (format stream "unknown command ~S" name)))))

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

(defun parse-command-line (commands program-name arguments)
  (declare (type hash-table commands)
           (type string program-name)
           (type list arguments)
           (ignore program-name))
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
                         (setf (gethash name option-values) t)))))
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
                      (pop program-arguments)))))))
      (parse-options)
      (cond
        (has-subcommands
         (when (zerop (length arguments))
           (error 'missing-command))
         (let ((name (pop arguments)))
           (setf command (or (gethash name commands)
                             (error 'unknown-command :name name))))
         (parse-options)
         (parse-arguments))
        (t
         (parse-arguments))))
    (values command option-values argument-values)))
