(in-package :netrc)

(defclass entry ()
  ((machine
    :type (or string null (member :default))
    :initarg :machine
    :initform nil
    :accessor entry-machine)
   (port
    :type (or integer null)
    :initform nil
    :accessor entry-port)
   (login
    :type (or string null)
    :initform nil
    :accessor entry-login)
   (password
    :type (or string null)
    :initform nil
    :accessor entry-password)
   (account
    :type (or string null)
    :initform nil
    :accessor entry-account)))

(defmethod print-object ((entry entry) stream)
  (print-unreadable-object (entry stream :type t)
    (with-slots (machine port login) entry
      (format stream "~@[~A~]~@[:~D~]" machine port))))
