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
    :initform nil
    :accessor shared-library-cflags)
   (ldflags
    :type list
    :initarg :ldflags
    :initform nil
    :accessor shared-library-ldflags)
   (libs
    :type list
    :initarg :libs
    :initform nil
    :accessor shared-library-libs))
  (:default-initargs
   :type "so")
  (:documentation
   "The ASDF component representing a shared library and the C source and header
files it is build from."))

(defun shared-library-filename (library)
  (declare (type shared-library library))
  (let ((name (asdf:component-name library)))
    (uiop:make-pathname* :name (concatenate 'string "lib" name)
                         :type (asdf:file-type library))))

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
          (t (error "unhandled shared library source type ~S" type)))))
    (let ((command (append (list compiler)
                           (list "-shared" "-fPIC")
                           (shared-library-cflags library)
                           (shared-library-ldflags library)
                           (list "-o" (namestring library-file))
                           (mapcar #'namestring c-source-files)
                           (shared-library-libs library))))
      (uiop:run-program command :force-shell t
                                :output *standard-output*
                                :error-output *error-output*))
    library-file))

(defmethod asdf:perform ((op asdf:load-op) (library shared-library))
  nil)

(defmethod asdf:input-files ((op asdf:compile-op) (library shared-library))
  (let ((directory (shared-library-source-directory library)))
    (with-slots (source-files header-files) library
      (mapcar (lambda (filename)
                (merge-pathnames filename directory))
              (append source-files header-files)))))

(defmethod asdf:output-files ((op asdf:compile-op) (library shared-library))
  (list (shared-library-filename library)))

(defun shared-library-source-directory (library)
  "Return the absolute path of the directory containing C source files for a
shared library ASDF component."
  (declare (type shared-library library))
  (let ((parent-directory
          (asdf:component-pathname (asdf:component-parent library))))
    (make-pathname
     :directory (append (pathname-directory parent-directory)
                        (list (asdf:component-name library))))))
