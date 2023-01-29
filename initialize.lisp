;; Create and use a temporary package to avoid polluting :CL-USER
(defpackage :tungsten-init
  (:use :cl)
  (:export
   :load-asdf))

(in-package :tungsten-init)

(defvar *log-output* *error-output*
  "The stream used to log information during the initialization process.")

(defun root-directory ()
  "Return the path of the directory containing the initialize.lisp file.

Signal an error if the initialization file was not loaded."
  (when (null *load-pathname*)
    (error "*LOAD-PATHNAME* is null, initialization file was not loaded"))
  (make-pathname :directory (pathname-directory *load-pathname*)))

(defun asdf-source-path ()
  "Locate and return the path of the ASDF source file bundled with cl-systems."
  (let ((root (root-directory)))
    (make-pathname :directory (pathname-directory root)
                   :name "asdf" :type "lisp")))

(defun normalize-path-string (path)
  "Transform a path string in a way that removes all characters which could
cause issues in a path component."
  (substitute-if
   #\_
   (lambda (c)
     (member c '(#\Space #\/ #\-) :test #'char=))
   (string-downcase path)))

(defun fasl-directory ()
  "Return the path of the directory used to store compiled files for the current
Common Lisp implementation and platform."
  (let* ((implementation (lisp-implementation-type))
         (version (lisp-implementation-version))
         (architecture (machine-type))
         (directory-name (format nil "~A-~A-~A"
                                 (normalize-path-string implementation)
                                 (normalize-path-string version)
                                 (normalize-path-string architecture)))
         (cache-path
           (make-pathname :directory
                          `(:relative ".cache" "common-lisp" "tungsten"
                                      ,directory-name))))
    (merge-pathnames cache-path (user-homedir-pathname))))

(defun asdf-fasl-path ()
  "Return the path of the compiled version of the ASDF source file bundled with
the tungsten repository.

The location of the file depends on runtime information to ensure we always
load a file which was compiled with the currently running Lisp
implementation."
  (let ((file-path (make-pathname :directory '(:relative "asdf")
                                  :name "asdf" :type "fasl")))
    (merge-pathnames file-path (fasl-directory))))

(defun load-asdf ()
  "Locate and load the copy of ASDF bundled with tungsten.

We use the asdf.lisp file which is produced by the ASDF build process, called
by the utils/update-asdf script.

Note that in the current state, we do not detect changes to the ASDF file.
This means that the compiled file must be manually deleted when updating
tungsten."
  (let ((asdf-source-path (asdf-source-path))
        (asdf-fasl-path (asdf-fasl-path)))
    (ensure-directories-exist asdf-fasl-path)
    (unless (probe-file asdf-fasl-path)
      (format *log-output* "compiling ~a to ~a~%"
              asdf-source-path asdf-fasl-path)
      (compile-file asdf-source-path :output-file asdf-fasl-path
                                     :verbose nil :print nil))
    (load asdf-fasl-path)))

;; Main
(in-package :cl-user)

;; For some reason, disabling sbcl compiler notes does not work when using
;; DECLARE in LOAD-ASDF. So we have to use DECLAIM here.
(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(tungsten-init:load-asdf)

;; Cleaning
(delete-package :tungsten-init)
