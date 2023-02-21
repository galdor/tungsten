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

(defun search-entries* (entries &key machine port login account)
  (declare (type list entries)
           (type (or string null) machine login account)
           (type (or (integer 1 65535) null) port))
  (flet ((match (value entry slot)
           (or (null value)
               (equal value (slot-value entry slot)))))
    (let ((matches nil))
      (dolist (entry entries (nreverse matches))
        (when (and (match machine entry 'machine)
                   (match port entry 'port)
                   (match login entry 'login)
                   (match account entry 'account))
          (push entry matches))))))
