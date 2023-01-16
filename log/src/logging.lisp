(in-package :log)

(deftype level ()
  '(member :debug :info :error))

(deftype domain-part ()
  '(or symbol string))

(deftype domain ()
  'list)

(defclass message ()
  ((time
    :type (integer 0)
    :initform (get-universal-time)
    :reader message-time)
   (domain
    :type (or domain null)
    :initarg :domain
    :initform nil
    :accessor message-domain)
   (level
    :type level
    :initarg :level
    :accessor message-level)
   (text
    :type string
    :initarg :text
    :accessor message-text)
   (data
    :type list
    :initarg :data
    :initform nil
    :accessor message-data)))
