(defsystem "tungsten-email"
  :description "Utilities to work with various email protocols."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-json"
   "tungsten-netrc"
   "tungsten-openssl"
   "tungsten-system"
   "tungsten-text"
   "tungsten-time"
   "tungsten-uuid")
  :pathname "src"
  :serial t
  :components
  ((:module
    "mime"
    :serial t
    :components
    ((:file "package")
     (:file "media-types")
     (:file "media-ranges")
     (:file "json")
     (:file "quoted-printable")))
   (:module
    "imf"
    :serial t
    :components
    ((:file "package")
     (:file "syntax")
     (:file "encoder")
     (:file "addresses")
     (:file "message-ids")
     (:file "messages")))
   (:module
    "smtp"
    :serial t
    :components
    ((:file "package")
     (:file "client")
     (:file "protocol")
     (:file "sasl")
     (:file "sasl-plain"))))
  :in-order-to ((test-op (test-op "tungsten-email/test"))))

(defsystem "tungsten-email/test"
  :description "Tests for the tungsten-email system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-email")
  :pathname "t"
  :serial t
  :components
  ((:module
    "mime"
    :serial t
    :components
    ((:file "package")))
   (:module
    "imf"
    :serial t
    :components
    ((:file "package")))
   (:module
    "smtp"
    :serial t
    :components
    ((:file "package"))))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :email-test)))
