(defsystem "tungsten-text/unicode-base"
  :description "Unicode utilities that must be available in the UCD package."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src/unicode"
  :serial t
  :components
  ((:file "package")
   (:file "unicode-base")))

(defsystem "tungsten-text"
  :description "Textual data manipulation."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :pathname "src"
  :defsystem-depends-on
  ("tungsten-asdf-utils")
  :depends-on
  ("tungsten-core"
   "tungsten-text/unicode-base"
   "tungsten-text/ucd")
  :serial t
  :components
  ((:module
    "unicode"
    :pathname "unicode"
    :serial t
    :components
    ((:file "unicode")
     ("asdf-utils:generated-cl-source"
      "ucd"
      :generation (:ucd :generate :unicode)
      :dependencies ("../ucd/" "../../data/ucd/"))))
   (:module
    "text"
    :pathname "text"
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
     (:file "macintosh")
     (:file "hex-encoding")
     (:file "base64")
     (:file "buffers"))))
  :in-order-to ((test-op (test-op "tungsten-text/test"))))

(defsystem "tungsten-text/ucd"
  :description "Unicode character database utilities."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on
  ("tungsten-core"
   "tungsten-text/unicode-base")
  :pathname "src/ucd"
  :serial t
  :components
  ((:file "package")
   (:file "files")
   (:file "data")
   (:file "generation")))

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
