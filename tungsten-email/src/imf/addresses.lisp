(in-package :imf)

(defclass mailbox ()
  ((display-name
    :type (or string null)
    :initarg :display-name
    :initform nil
    :accessor mailbox-display-name)
   (local-part
    :type string
    :initarg :local-part
    :accessor mailbox-local-part)
   (domain
    :type string
    :initarg :domain
    :accessor mailbox-domain)))

(defclass group ()
  ((display-name
    :type string
    :initarg :display-name
    :accessor group-display-name)
   (mailboxes
    :type list
    :initarg :mailboxes
    :initform nil
    :accessor group-mailboxes)))

(deftype address ()
  '(or mailbox group))

(defun make-mailbox (local-part domain &key display-name)
  (declare (type string local-part domain)
           (type (or string null) display-name))
  (make-instance 'mailbox :local-part local-part
                          :domain domain
                          :display-name display-name))

(defun make-group (display-name mailboxes)
  (declare (type string display-name)
           (type list mailboxes))
  (make-instance 'group :display-name display-name
                        :mailboxes mailboxes))

(defun serialize-address (address)
  (declare (type address address))
  (etypecase address
    (mailbox
     (serialize-mailbox address))
    (group
     (serialize-group address))))

(defun serialize-mailbox (mailbox)
  (declare (type mailbox mailbox))
  (with-slots (display-name local-part domain) mailbox
    (let ((address-specification
            (serialize-address-specification local-part domain)))
      (cond
        (display-name
         (concatenate 'string display-name " <" address-specification ">"))
        (t
         address-specification)))))

(defun serialize-group (group)
  (declare (type group group))
  (with-slots (display-name mailboxes) group
    (with-output-to-string (stream)
      (write-string display-name stream)
      (write-string ": " stream)
      (let ((i 0))
        (dolist (mailbox mailboxes)
          (when (> i 0)
            (write-string ", " stream))
          (write-string (serialize-mailbox mailbox) stream)
          (incf i)))
      (write-char #\; stream))))

(defun serialize-address-specification (local-part domain)
  (declare (type string local-part domain))
  (concatenate 'string local-part "@" domain))
