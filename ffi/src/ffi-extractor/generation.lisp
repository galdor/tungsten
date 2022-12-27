(in-package :ffi-extractor)

(defparameter *header-files*
  (list "stdbool.h"
        "stddef.h"                      ; needed for offsetof()
        "stdio.h"
        "inttypes.h"
        "stdlib.h"))

(defun generate-c-program (manifest &key (stream *standard-output*)
                                         (package :cl-user)
                                    &aux (*print-case* :downcase))
  (let ((header-files (manifest-header-files manifest)))
    (dolist (file (append *header-files* header-files))
      (format stream "#include <~A>~%" file))
    (write-char #\newline stream))
  (format stream "#define FFI_MEMBER_COUNT(s_, m_) ~
                  ((sizeof (((s_ *)NULL)->m_)) / ~
                   (sizeof (((s_ *)NULL)->m_[0])))~%")
  (format stream "~%#define FFI_STRING0(e_) #e_~%")
  (format stream "~%#define FFI_STRING(e_) FFI_STRING0(e_)~%")
  (format stream "~%const char *~%ffi_convert_type(const char *name, ~
                      size_t sz, bool is_signed) {~%~
                  if (sz == 1 && is_signed) return \":int8\";~%~
                  else if (sz == 2 && is_signed) return \":int16\";~%~
                  else if (sz == 4 && is_signed) return \":int32\";~%~
                  else if (sz == 8 && is_signed) return \":int64\";~%~
                  else if (sz == 1 && !is_signed) return \":uint8\";~%~
                  else if (sz == 2 && !is_signed) return \":uint16\";~%~
                  else if (sz == 4 && !is_signed) return \":uint32\";~%~
                  else if (sz == 8 && !is_signed) return \":uint64\";~%~

                  fprintf(stderr, \"unsupported type %s\n\", name);~%~
                  exit(1);~%~
                 }~%")
  (write-char #\newline stream)
  (format stream "~%int main(int argc, char **argv) {~%")
  (format stream "puts(\";;;; This file has been generated by the cl-ffi ~
                         extractor.\");~%~%")
  (format stream "puts(\"\\n(in-package ~S)\");" package)
  (dolist (form manifest)
    (write-char #\newline stream)
    (case (car form)
      (include
       nil)
      (constant
       (generate-constant (cdr form) stream))
      (type
       (generate-c-type (cdr form) stream))
      (enum
       (generate-c-enum (cdr form) stream))
      (bitset
       (generate-c-bitset (cdr form) stream))
      (struct
       (generate-c-struct (cdr form) stream))
      (t
       (error "invalid manifest form ~S" form))))
  (format stream "}~%"))

(defun generate-constant (form stream)
  (destructuring-bind (name c-name &optional (base-type :int)) form
    (format stream "~%printf(\"\\n(defconstant ~A \"~A\")\\n\",~
                             ~A);~%"
            name (integer-printf-string base-type) c-name)))

(defun generate-c-type (form stream)
  (destructuring-bind (name c-name) form
    (format stream "~%printf(\"\\n(ffi:define-type-alias ~A %s)\\n\", ~
                             ffi_convert_type(FFI_STRING(~A), ~
                             sizeof(~A), ~
                             ((~A)-1) < 0));~%"
            name c-name c-name c-name)))

(defun generate-c-enum (form stream)
  (destructuring-bind ((name base-type) (&rest constants)) form
    (format stream "puts(\"\\n(ffi:define-enum (~A :base-type ~S)\");~%"
            name base-type)
    (format stream "puts(\"(\");~%")
    (dolist (constant constants)
      (destructuring-bind (constant-name value) constant
        (format stream "printf(\"  (~S \"~A\")\\n\", (~A)~A);~%"
                constant-name (integer-printf-string base-type)
                (integer-type-name base-type) value)))
    (format stream "puts(\"))\");~%")))

(defun generate-c-bitset (form stream)
  (destructuring-bind ((name base-type) (&rest constants)) form
    (format stream "puts(\"\\n(ffi:define-bitset (~A :base-type ~S)\");~%"
            name base-type)
    (format stream "puts(\"(\");~%")
    (dolist (constant constants)
      (destructuring-bind (constant-name value) constant
        (format stream "printf(\"  (~S \"~A\")\\n\", (~A)~A);~%"
                constant-name (integer-printf-string base-type)
                (integer-type-name base-type) value)))
    (format stream "puts(\"))\");~%")))

(defun generate-c-struct (form stream)
  (destructuring-bind ((name c-name) (&rest members)) form
    (format stream "printf(\"\\n(ffi:define-struct (~A :size %zu)\", ~
                           sizeof(~A));~%"
            name c-name)
    (format stream "puts(\"(\");~%")
    (dolist (member members)
      (destructuring-bind (member-name type member-c-name &key (count 1))
          member
        (let ((count-expr (if (eq count 'auto)
                              (format nil "FFI_MEMBER_COUNT(~A, ~A)"
                                      c-name member-c-name)
                              (format nil "(size_t)~D" count))))
          (format stream "printf(\"  (~S ~S :count %Zu :offset %Zu)\\n\", ~
                                 ~A, offsetof(~A, ~A));~%"
                  member-name type
                  count-expr c-name member-c-name))))
    (format stream "puts(\"))\");~%")))

(defun integer-printf-string (base-type)
  (ecase base-type
    (:char "\"%hhd\"")
    (:unsigned-char "\"hhu\"")
    (:short "\"%hu\"")
    (:unsigned-short "\"%hd\"")
    (:int "\"%d\"")
    (:unsigned-int "\"%u\"")
    (:long "\"%ld\"")
    (:unsigned-long "\"%lu\"")
    (:long-long "\"%lld\"")
    (:unsigned-long-long "\"%llu\"")
    (:int8 "\"%\"PRId8")
    (:uint8 "\"%\"PRIu8")
    (:int16 "\"%\"PRId16")
    (:uint16 "\"%\"PRIu16")
    (:int32 "\"%\"PRId32")
    (:uint32 "\"%\"PRIu32")
    (:int64 "\"%\"PRId64")
    (:uint64 "\"%\"PRIu64")))

(defun integer-type-name (base-type)
  (ecase base-type
    (:char "char")
    (:unsigned-char "unsigned char")
    (:short "short")
    (:unsigned-short "unsigned short")
    (:int "int")
    (:unsigned-int "unsigned int")
    (:long "long")
    (:unsigned-long "unsigned long")
    (:long-long "long long")
    (:unsigned-long-long "unsigned long long")
    (:int8 "int8_t")
    (:uint8 "uint8_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int64 "int64_t")
    (:uint64 "uint64_t")))
