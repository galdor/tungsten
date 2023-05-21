(in-package :sqlite)

;;; There almost no scenario in which using a SQLite built without
;;; SQLITE_THREADSAFE is going to end up well.
(when (zerop (sqlite3-threadsafe))
  (error "SQLite was not compiled with SQLITE_THREADSAFE."))

(defun library-version ()
  (sqlite3-libversion))
