(in-package :system)

(defun %count-file-descriptors ()
  (kinfo-getfile (getpid)))

(defun %memory-usage ()
  (let ((page-size (sysconf :sc-pagesize)))
    #+freebsd
    (with-kvm-openfiles (%kvm nil "/dev/null" nil nil)
      (do-kvm-getprocs (%proc %kvm :kern-proc-pid (getpid))
        (let ((virtual
                (ffi:foreign-structure-member %proc 'kinfo-proc :ki-size))
              (resident
                (* (ffi:foreign-structure-member %proc 'kinfo-proc :ki-rssize)
                   page-size)))
          (return-from %memory-usage (values virtual resident)))))
    #-freebsd
    (unsupported-feature "memory usage inspection")))
