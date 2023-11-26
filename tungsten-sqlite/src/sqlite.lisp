(in-package :sqlite)

(defvar *database* nil)

(deftype database-access-mode ()
  '(member :read-only :read-write))

(defclass database ()
  ((path
    :type (or pathname string)
    :initarg :path
    :reader database-path)
   (%database
    :type ffi:pointer
    :initarg :%database
    :reader database-%database)))

(defmethod print-object ((db database) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~A" (database-path db))))

;;; There almost no scenario in which using a SQLite built without
;;; SQLITE_THREADSAFE is going to end up well.
(when (zerop (sqlite3-threadsafe))
  (error "SQLite was not compiled with SQLITE_THREADSAFE"))

(defun library-version ()
  (sqlite3-libversion))

(defun open-database (path &key (mode :read-write) in-memory)
  (declare (type (or pathname string) path)
           (type database-access-mode mode)
           (type boolean in-memory))
  (let ((path-string (namestring path))
        (flags (ecase mode
                 (:read-only (list :readonly))
                 (:read-write (list :readwrite :create)))))
    (when in-memory
      (push :memory flags))
    (let ((%database (sqlite3-open-v2 path-string flags nil)))
      (core:abort-protect
          (progn
            (sqlite3-extended-result-codes %database t)
            (make-instance 'database :path path
                                     :%database %database))
        (sqlite3-close-v2 %database)))))

(defun close-database (db)
  (declare (type database db))
  (with-slots (%database) db
    (sqlite3-close-v2 %database)))

(defmacro with-database ((path &rest options) &body body)
  `(let ((*database* (open-database ,path ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close-database *database*))))

(defmacro do-query-rows ((row query &optional parameters
                                    &key (database *database*))
                         &body body)
  (declare #+sbcl (sb-ext:muffle-conditions style-warning))
  (let ((%stmt (gensym "%STMT-")))
    `(let ((,%stmt (sqlite3-prepare-v3 (database-%database ,database)
                                       ,query nil)))
       (unwind-protect
            (progn
              (bind-statement-parameters ,%stmt ,parameters)
              (loop
                (unless (sqlite3-step ,%stmt)
                  (return))
                (let ((,row (read-statement-row ,%stmt)))
                  ,@body)))
         (sqlite3-finalize ,%stmt)))))

(defun query (query &optional parameters
                    &key (database *database*))
  (declare (type string query)
           (type sequence parameters)
           (type database database)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (let ((results nil))
    (do-query-rows (row query parameters :database database)
      (push row results))
    (nreverse results)))

(defun query-row (query &optional parameters
                        &key (database *database*))
  (declare (type string query)
           (type sequence parameters)
           (type database database)
           #+sbcl (sb-ext:muffle-conditions style-warning))
  (do-query-rows (row query parameters :database database)
    (return-from query-row row)))

(defun bind-statement-parameters (%stmt parameters)
  (declare (type ffi:pointer %stmt)
           (type sequence parameters))
  (let ((parameter-vector (coerce parameters 'vector)))
    (dotimes (i (length parameter-vector))
      (bind-statement-parameter %stmt (1+ i) (aref parameter-vector i)))))

(defun bind-statement-parameter (%stmt i parameter)
  (declare (type ffi:pointer %stmt)
           (type (integer 1) i))
  (let ((parameter-type (query-parameter-type parameter))
        (parameter-value (if (consp parameter) (cdr parameter) parameter)))
    (ecase parameter-type
      (:null
       (sqlite3-bind-null %stmt i))
      (:integer
       (sqlite3-bind-int64 %stmt i parameter-value))
      (:float
       (sqlite3-bind-double %stmt i (float parameter-value 0.0d0)))
      (:text
       (sqlite3-bind-text %stmt i parameter-value))
      (:blob
       (sqlite3-bind-blob64 %stmt i parameter-value)))))

(defun read-statement-row (%stmt)
  (declare (type ffi:pointer %stmt))
  (let* ((nb-columns (sqlite3-data-count %stmt))
         (row (make-array nb-columns)))
    (dotimes (i nb-columns row)
      (setf (aref row i) (read-statement-column %stmt i)))))

(defun read-statement-column (%stmt index)
  (declare (type ffi:pointer %stmt)
           (type (integer 0) index))
  (ecase (sqlite3-column-type %stmt index)
    (:null
     nil)
    (:integer
     (sqlite3-column-int64 %stmt index))
    (:float
     (sqlite3-column-double %stmt index))
    (:text
     (sqlite3-column-text %stmt index))
    (:blob
     (sqlite3-column-blob %stmt index))))

(defun query-parameter-type (value)
  (cond
    ((null value)
     :null)
    ((consp value)
     (car value))
    ((integerp value)
     :integer)
    ((floatp value)
     :float)
    ((stringp value)
     :text)
    ((arrayp value)
     :blob)))
