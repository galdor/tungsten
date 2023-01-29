(in-package :log)

(deftype level ()
  '(member :debug :info :error))

(deftype domain-part ()
  '(or symbol string))

(deftype domain ()
  'list)
