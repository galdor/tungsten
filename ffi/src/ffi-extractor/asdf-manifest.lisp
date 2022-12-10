(in-package :ffi-extractor)

(defclass manifest (asdf:cl-source-file)
  ((package
    :type keyword
    :initarg :package
    :initform :cl-user
    :accessor manifest-package)
   (compiler
    :type (or string pathname)
    :initarg :compiler
    :initform "cc"
    :accessor manifest-compiler)
   (cflags
    :type list
    :initarg :cflags
    :initform nil
    :accessor manifest-cflags)
   (ldflags
    :type list
    :initarg :ldflags
    :initform nil
    :accessor manifest-ldflags)
   (libs
    :type list
    :initarg :libs
    :initform nil
    :accessor manifest-libs))
  (:default-initargs
   :type "ffim"))

(defclass process-manifest (asdf:downward-operation)
  ()
  (:documentation "An ASDF operation which processes a FFI manifest and generate a Lisp source file."))

(defmethod asdf:input-files ((op process-manifest) (manifest manifest))
  (list (asdf:component-pathname manifest)))

(defmethod asdf:output-files ((op process-manifest) (manifest manifest))
  (let* ((input-file (car (asdf:input-files op manifest)))
         (output-file (make-pathname :defaults input-file :type "lisp")))
    (list output-file)))

(defmethod asdf:input-files ((op asdf:compile-op) (manifest manifest))
  (asdf:output-files 'process-manifest manifest))

(defmethod asdf:component-depends-on ((op process-manifest)
                                      (manifest manifest))
  `((asdf:prepare-op ,manifest) ,@(call-next-method)))

(defmethod asdf:component-depends-on ((op asdf:compile-op)
                                      (manifest manifest))
  `((process-manifest ,manifest) ,@(call-next-method)))

(defmethod asdf:component-depends-on ((op asdf:load-source-op)
                                      (manifest manifest))
  `((process-manifest ,manifest) ,@(call-next-method)))

(defmethod asdf:perform ((op process-manifest) (manifest manifest))
  (let ((ffim-path (car (asdf:input-files op manifest)))
        (lisp-path (asdf:output-file op manifest)))
    (with-slots (package compiler cflags ldflags libs) manifest
      (extract ffim-path :output-path lisp-path
                         :package package
                         :compiler compiler
                         :cflags cflags
                         :ldflags ldflags
                         :libs libs))))
