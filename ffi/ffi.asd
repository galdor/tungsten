(defsystem "ffi"
  :description "A foreign function interface."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :depends-on ("asdf-utils" "text")
  :pathname "src"
  :serial t
  :components
  ((:module
    "ffi"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "ffi-ccl" :if-feature :ccl)
     (:file "ffi-sbcl" :if-feature :sbcl)
     (:file "ffi")
     (:file "enumerations")
     (:file "bitsets")
     (:file "structures")
     (:file "unions")
     (:file "libraries")
     (:file "errno")))
   (:module "ffi-extractor"
    :serial t
    :components
    ((:file "package")
     (:file "manifest")
     (:file "generation")
     (:file "extraction")
     (:file "asdf-manifest"))))
  :in-order-to ((test-op (test-op "ffi/test"))))

(defsystem "ffi/test"
  :description "Tests for the ffi system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("asdf-utils")
  :depends-on ("check" "ffi")
  :pathname "t"
  :serial t
  :components
  ((:file "package")
   ("asdf-utils:shared-library"
    "ffi-test"
    :source-files ("ffi-test.c")
    :header-files ("ffi-test.h")
    :cflags ("std=c99" "Wall" "Wextra" "Werror" "Wsign-conversion"))
   (:file "ffi-test-definitions")
   (:file "memory")
   (:file "foreign-values")
   (:file "foreign-funcall")
   (:file "callbacks")
   (:file "structures")
   (:file "unions")
   (:file "errno"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :ffi-test)))
