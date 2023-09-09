(defsystem "tungsten-text"
  :description "Textual data manipulation."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :depends-on
  ("tungsten-core")
  :serial t
  :components
  ((:file "package")
   (:file "base")
   (:file "character-encodings")
   (:file "strings")
   (:file "external-formats")
   (:file "character-mapping")
   (:file "ascii")
   (:file "utf-8")
   (:file "utf-16")
   (:file "iso-8859-1")
   (:file "hex-encoding")
   (:file "base64")
   (:file "buffers"))
  :in-order-to ((test-op (test-op "tungsten-text/test"))))

(defsystem "tungsten-text/test"
  :description "Tests for the text system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-test"
   "tungsten-text")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "ascii")
   (:file "utf-8")
   (:file "utf-16")
   (:file "iso-8859-1")
   (:file "hex-encoding")
   (:file "base64"))
  :perform (test-op (op system)
                    (symbol-call :test :run :package :text-test)))
