(defsystem "ffi"
  :description "A foreign function interface."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :license "ISC"
  :depends-on ("text")
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "types")
   (:file "ffi-ccl" :if-feature :ccl)
   (:file "ffi-sbcl" :if-feature :sbcl)
   (:file "libraries")
   (:file "ffi")
   (:file "errno"))
  :in-order-to ((test-op (test-op "ffi/test"))))

(defsystem "ffi/test"
  :description "Tests for the ffi system."
  :author "Nicolas Martyanoff <nicolas@n16f.net>"
  :defsystem-depends-on ("asdf-utils")
  :depends-on ("check" "ffi")
  :pathname "t"
  :serial t
  :components
  (("asdf-utils:shared-library"
    "ffi-test"
    :source-files ("ffi-test.c")
    :header-files ("ffi-test.h")
    :cflags ("std=c99" "Wall" "Wextra" "Werror" "Wsign-conversion"))
   (:file "package")
   (:file "foreign-values")
   (:file "foreign-funcall")
   (:file "errno"))
  :perform (test-op (op system)
                    (symbol-call :check :run :package :ffi-test)))
