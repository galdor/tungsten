(in-package :netrc-test)

(defun entries (&rest lines)
  (netrc:parse-entries
   (with-output-to-string (stream)
     (dolist (line lines)
       (write-string line stream)
       (write-char #\Newline stream)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-parser-test ((name entries &rest lines) &body body)
    `(deftest
         ,(intern (concatenate 'string "PARSE-ENTRIES/" (symbol-name name)))
         ()
       (let ((,entries (entries ,@lines)))
         ,@body))))

(define-parser-test (empty-string
                     entries
                     "")
  (check-null entries))

(define-parser-test (default
                     entries
                     "default")
  (check= 1 (length entries))
  (let ((entry (first entries)))
    (check-eq :default (netrc:entry-machine entry))))

(define-parser-test (default-and-machine
                     entries
                     "default machine example.com")
  (check= 2 (length entries))
  (let ((entry (first entries)))
    (check-eq :default (netrc:entry-machine entry)))
  (let ((entry (second entries)))
    (check-equal "example.com" (netrc:entry-machine entry))))

(define-parser-test (partial-entry
                     entries
                     "machine example.com"
                     "login bob")
  (check= 1 (length entries))
  (let ((entry (first entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-null (netrc:entry-port entry))
    (check-equal "bob" (netrc:entry-login entry))
    (check-null (netrc:entry-password entry))
    (check-null (netrc:entry-account entry))))

(define-parser-test (full-entry
                     entries
                     "machine example.com"
                     "port 42"
                     "login bob"
                     "password blabla"
                     "account foobar")
  (check= 1 (length entries))
  (let ((entry (first entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-eql 42 (netrc:entry-port entry))
    (check-equal "bob" (netrc:entry-login entry))
    (check-equal "blabla" (netrc:entry-password entry))
    (check-equal "foobar" (netrc:entry-account entry))))

(define-parser-test (full-entry-with-spacing
                     entries
                     "	"
                     ""
                     "machine	example.com"
                     "  port 42  "
                     "login   bob"
                     ""
                     "		password blabla"
                     "account foobar		"
                     "   ")
  (check= 1 (length entries))
  (let ((entry (first entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-eql 42 (netrc:entry-port entry))
    (check-equal "bob" (netrc:entry-login entry))
    (check-equal "blabla" (netrc:entry-password entry))
    (check-equal "foobar" (netrc:entry-account entry))))

(define-parser-test (multiple-entries
                     entries
                     "machine example.com login bob"
                     "machine example.com login alice"
                     "machine example.com login eve"
                     "default login arthur")
  (check= 4 (length entries))
  (let ((entry (first entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-equal "bob" (netrc:entry-login entry)))
  (let ((entry (second entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-equal "alice" (netrc:entry-login entry)))
  (let ((entry (third entries)))
    (check-equal "example.com" (netrc:entry-machine entry))
    (check-equal "eve" (netrc:entry-login entry)))
  (let ((entry (fourth entries)))
    (check-equal :default (netrc:entry-machine entry))
    (check-equal "arthur" (netrc:entry-login entry))))

(deftest parse-entries/invalid-data ()
  (macrolet ((check-invalid (condition &rest lines)
               `(check-signals ,condition
                               (netrc:parse-entries (entries ,@lines)))))
    (check-invalid netrc:invalid-token
                   "machine example.com foo")
    (check-invalid netrc:invalid-port-number
                   "machine example.com port foo")
    (check-invalid netrc:invalid-port-number
                   "machine example.com port 123456")
    (check-invalid netrc:missing-token
                   "machine")
    (check-invalid netrc:missing-token
                   "machine example.com login")
    (check-invalid netrc:orphaned-token
                   "login bob")))
