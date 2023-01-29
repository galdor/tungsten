(setf *compile-verbose* nil)
(setf *load-verbose* nil)

;; Load and initialize ASDF
(let* ((current-directory
         #+ccl (ccl:current-directory)
         #-ccl *default-pathname-defaults*)
       (load-pathname
         (merge-pathnames *load-pathname* current-directory))
       (root-directory
         (make-pathname
          :directory (butlast (pathname-directory load-pathname)))))
  (asdf:clear-source-registry)
  (asdf:initialize-source-registry
   (list :source-registry
         :ignore-inherited-configuration
         (list :tree root-directory))))

;; Find and load all Tungsten test systems
(asdf:load-system "tungsten-asdf-utils")
(let* ((tungsten-prefix "tungsten-")
       (test-suffix "/test")
       (min-length (+ (length tungsten-prefix)
                      (length test-suffix))))
  (mapc
   (lambda (system)
     (let ((name (asdf:component-name system)))
       (when (and (> (length name) min-length)
                  (string= name tungsten-prefix
                           :end1 (length tungsten-prefix))
                  (string= name test-suffix
                           :start1 (- (length name) (length test-suffix))))
         (asdf:load-system name))))
   (asdf-utils:list-systems)))

;; Run all tests
(handler-case
    (progn
      (test:run)
      (uiop:quit 0))
  (error (condition)
    (declare (ignore condition))
    (uiop:quit 1)))
