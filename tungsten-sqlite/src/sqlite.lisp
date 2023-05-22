(in-package :sqlite)

(deftype database-access-mode ()
  '(member :read-only :read-write))

(defclass database ()
  ((path
    :type (or pathname string)
    :initarg :path
    :reader database-path)
   (%database
    :type ffi:pointer
    :initarg :%database)))

(defmethod print-object ((db database) stream)
  (print-unreadable-object (db stream :type t)
    (format stream "~A" (database-path db))))

;;; There almost no scenario in which using a SQLite built without
;;; SQLITE_THREADSAFE is going to end up well.
(when (zerop (sqlite3-threadsafe))
  (error "SQLite was not compiled with SQLITE_THREADSAFE."))

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
          (make-instance 'database :path path
                                   :%database %database)
        (sqlite3-close-v2 %database)))))

(defun close-database (db)
  (declare (type database db))
  (with-slots (%database) db
    (sqlite3-close-v2 %database)))

(defmacro with-database ((db path &rest options) &body body)
  `(let ((,db (open-database ,path ,@options)))
     (unwind-protect
          (progn
            ,@body)
       (close-database db))))
