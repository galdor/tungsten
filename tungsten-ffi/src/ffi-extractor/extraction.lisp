(in-package :ffi-extractor)

(defparameter *cflags*
  '("-Wall" "-Werror"
    "-I/usr/local/include"))

(defparameter *ldflags*
  '("-L /usr/local/lib"))

(defun extract (manifest-path &key output-path
                                   c-program-path
                                   executable-path
                                   (package :cl-user)
                                   (compiler "cc") cflags ldflags libs)
  (unless output-path
    (setf output-path (make-pathname :defaults manifest-path :type "lisp")))
  (let* ((manifest
           (load-manifest manifest-path :package (find-package package)))
         (c-program-path
           (or c-program-path
               (make-pathname :defaults manifest-path :type "c")))
         (executable-path
           (or executable-path
               (make-pathname :defaults manifest-path :type nil)))
         (cflags-args
           (mapcar (lambda (flag) (concatenate 'string "-" flag))
                   (append *cflags* cflags)))
         (ldflags-args
           (mapcar (lambda (flag) (concatenate 'string "-" flag))
                   (append *ldflags* ldflags)))
         (libs-args
           (mapcar (lambda (flag) (concatenate 'string "-l" flag)) libs)))
    ;; Generate the program
    (with-open-file (output c-program-path :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
      (generate-c-program manifest :stream output :package package))
    ;; Execute the compiler to build the program
    (let ((command (append (list compiler)
                           cflags-args
                           ldflags-args
                           (list "-o" (namestring executable-path))
                           (list (namestring c-program-path))
                           libs-args)))
      (uiop:run-program command :force-shell t
                                :output nil :error-output *error-output*))
    ;; Execute the program to generate the Lisp source file
    (uiop:run-program (namestring executable-path)
                      :output output-path :error-output *error-output*)))
