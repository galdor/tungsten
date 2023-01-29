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
    :accessor manifest-libs)
   (asdf-shared-libraries
    :type list
    :initarg :asdf-shared-libraries
    :initform nil
    :accessor manifest-asdf-shared-libraries))
  (:default-initargs
   :type "ffim"))

(defclass process-manifest (asdf:downward-operation)
  ()
  (:documentation "An ASDF operation which processes a FFI manifest and generate a Lisp source file."))

(defmethod asdf:input-files ((op process-manifest) (manifest manifest))
  (list (asdf:component-pathname manifest)))

(defmethod asdf:output-files ((op process-manifest) (manifest manifest))
  (let ((input-file (car (asdf:input-files op manifest))))
    (list (make-pathname :defaults input-file :type "lisp")
          (make-pathname :defaults input-file :type "c")
          (make-pathname :defaults input-file :type nil))))

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
        (output-paths (asdf:output-files op manifest)))
    (with-slots (package compiler cflags ldflags libs
                 asdf-shared-libraries)
        manifest
      (destructuring-bind (asdf-libs-cflags asdf-libs-ldflags asdf-libs-libs)
          (asdf-shared-libraries-flags asdf-shared-libraries)
        (declare (ignore asdf-libs-ldflags asdf-libs-libs))
        (unless (find-package package)
          (error "unknown package ~S" package))
        (extract ffim-path :output-path (first output-paths)
                           :c-program-path (second output-paths)
                           :executable-path (third output-paths)
                           :package package
                           :compiler compiler
                           :cflags (append cflags asdf-libs-cflags)
                           :ldflags ldflags
                           :libs libs)))))

(defun asdf-shared-library-flags (spec)
  (destructuring-bind (system-name component-name) spec
    (let ((system (asdf:find-system system-name)))
    (unless system
      (error "unknown ASDF system ~S" system-name))
    (let ((component (asdf:find-component system component-name)))
      (unless component
        (error "unknown ASDF component ~S in system ~S"
               component-name system-name))
      (let* ((output-path (car (asdf:output-files 'asdf:compile-op component)))
             (lib-dir
               (make-pathname
                :directory (pathname-directory output-path)))
             (header-dir
               (asdf-utils:shared-library-source-directory component)))
        (list (list (concatenate 'string "I" (namestring header-dir)))
              (list (concatenate 'string "L" (namestring lib-dir)))
              (list component-name)))))))

(defun asdf-shared-libraries-flags (specs)
  (let ((cflags nil)
        (ldflags nil)
        (libs nil))
    (dolist (spec specs)
      (let ((flags (asdf-shared-library-flags spec)))
        (setf cflags (append cflags (first flags)))
        (setf ldflags (append ldflags (second flags)))
        (setf libs (append libs (third flags)))))
    (list (nreverse cflags) (nreverse ldflags) (nreverse libs))))
