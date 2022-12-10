(in-package :asdf-utils)

(defclass shared-library (asdf:file-component)
  ((source-files
    :type list
    :initarg :source-files
    :initform nil)
   (header-files
    :type list
    :initarg :header-files
    :initform nil)
   (compiler
    :type (or string pathname)
    :initarg :compiler
    :initform "cc")
   (cflags
    :type list
    :initarg :cflags
    :initform nil)
   (ldflags
    :type list
    :initarg :ldflags
    :initform nil)
   (libs
    :type list
    :initarg :libs
    :initform nil))
  (:default-initargs
   :type "so")
  (:documentation
   "The ASDF component representing a shared library and the C source and header
files it is build from."))

(defmethod shared-library-filename ((library shared-library))
  (let ((name (asdf:component-name library)))
    (uiop:make-pathname* :name (concatenate 'string "lib" name)
                         :type (asdf:file-type library))))

(defmethod shared-library-cflags-arguments ((library shared-library))
  (with-slots (cflags) library
    (mapcar (lambda (flag)
              (concatenate 'string "-" flag))
            cflags)))

(defmethod shared-library-ldflags-arguments ((library shared-library))
  (with-slots (ldflags) library
    (mapcar (lambda (flag)
              (concatenate 'string "-" flag))
            ldflags)))

(defmethod shared-library-libs-arguments ((library shared-library))
  (with-slots (libs) library
    (mapcar (lambda (lib)
              (concatenate 'string "-l" lib))
            libs)))

(defmethod asdf:perform ((op asdf:compile-op) (library shared-library))
  (let* ((library-file (asdf:output-file op library))
         (c-source-files nil)
         (c-header-files nil)
         (compiler (slot-value library 'compiler)))
    (dolist (input (asdf:input-files op library))
      (let ((type (pathname-type input)))
        (cond
          ((string= type "c") (push input c-source-files))
          ((string= type "h") (push input c-header-files))
          (t (error "Unhandled shared library source type ~S." type)))))
    (let ((command (append (list compiler)
                           (list "-shared" "-fPIC")
                           (shared-library-cflags-arguments library)
                           (shared-library-ldflags-arguments library)
                           (list "-o" (namestring library-file))
                           (mapcar #'namestring c-source-files)
                           (shared-library-libs-arguments library))))
      (uiop:run-program command :force-shell t
                                :output *standard-output*
                                :error-output *error-output*))
    library-file))

(defmethod asdf:perform ((op asdf:load-op) (library shared-library))
  nil)

(defmethod asdf:input-files ((op asdf:compile-op) (library shared-library))
  (let* ((parent-directory
           (asdf:component-pathname (asdf:component-parent library)))
         (directory
           (make-pathname
            :directory (append (pathname-directory parent-directory)
                               (list (asdf:component-name library))))))
    (with-slots (source-files header-files) library
      (mapcar (lambda (filename)
                (merge-pathnames filename directory))
              (append source-files header-files)))))

(defmethod asdf:output-files ((op asdf:compile-op) (library shared-library))
  (list (shared-library-filename library)))
