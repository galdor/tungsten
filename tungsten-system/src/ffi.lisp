(in-package :system)

;;;
;;; Libraries
;;;

#+freebsd
(ffi:use-foreign-library 'kvm "libkvm.so")

;;;
;;; Environment
;;;

(defun getpid ()
  (system-funcall ("getpid" (() pid-t))))

(defun gethostname ()
  ;; HOST_NAME_MAX is not available on FreeBSD for questionable reasons, so we
  ;; use a large value and check for truncation.
  ;;
  ;; Note that we cannot use uname() because the size of the character arrays
  ;; in the ustname structure is not specified.
  (let ((hostname-length 256))
    (ffi:with-foreign-value (%hostname :char :count hostname-length)
      (ffi:clear-foreign-memory %hostname hostname-length)
      (system-funcall
       ("gethostname" ((:pointer size-t) :int) %hostname hostname-length))
      (unless (zerop (ffi:foreign-value %hostname :char (1- hostname-length)))
        (error "System function \"gethostname\" returned a truncated ~
                hostname."))
      (ffi:decode-foreign-string %hostname))))

(defun getrlimit (resource)
  (ffi:with-foreign-value (%limit 'rlimit)
    (ffi:clear-foreign-memory %limit (ffi:foreign-type-size 'rlimit))
    (system-funcall ("getrlimit" ((resource :pointer) :int) resource %limit))
    (ffi:with-foreign-structure-members (((current :rlim-cur)
                                          (max :rlim-max))
                                         %limit 'rlimit)
      (values current max))))

(defun sysconf (name)
  (system-funcall ("sysconf" ((sysconf-name) :long) name)))

(defun getenv (name)
  (ffi:with-foreign-string (%name name)
    (let ((%string (system-funcall ("getenv" ((:pointer) :pointer) %name)
                                   :errorp nil)))
      (unless (ffi:null-pointer-p %string)
        (ffi:decode-foreign-string %string)))))

#+bsd
(defun kinfo-getfile (pid)
  (ffi:with-foreign-value (%count :int)
    (let ((%info (system-funcall
                  ("kinfo_getfile" ((pid-t :pointer) :pointer) pid %count))))
      (system-funcall ("free" ((:pointer) :void) %info)))
    (ffi:foreign-value %count :int)))


#+freebsd
(defmacro with-kvm-openfiles ((%kvm exec-file core-file swap-file flags)
                              &body body)
  (let ((%exec-file (gensym "%EXEC-FILE-"))
        (%core-file (gensym "%CORE-FILE-"))
        (%swap-file (gensym "%SWAP-FILE-"))
        (%error-buf (gensym "%ERROR-BUF-")))
    `(ffi:with-foreign-strings ((,%exec-file ,exec-file)
                                (,%core-file ,core-file)
                                (,%swap-file ,swap-file))
       (ffi:with-foreign-value (,%error-buf :char :count 1024)
         (ffi:clear-foreign-memory ,%error-buf 1024)
         (let ((,%kvm
                 (system-funcall
                  ("kvm_openfiles"
                   ((:pointer :pointer :pointer kvm-openfiles-flags :pointer)
                    :pointer)
                   ,%exec-file ,%core-file ,%swap-file ,flags ,%error-buf)
                  :error-value-function
                  (lambda (return-value)
                    (declare (ignore return-value))
                    nil)
                  :error-description-function
                  (lambda (error-value)
                    (declare (ignore error-value))
                    (ffi:decode-foreign-string ,%error-buf)))))
           (unwind-protect
                (progn
                  ,@body)
             (system-funcall ("kvm_close" ((:pointer) :int) ,%kvm))))))))

#+freebsd
(defmacro do-kvm-getprocs ((%proc %kvm op arg) &body body)
  (let ((%count (gensym "%COUNT-"))
        (kinfo-proc-size (gensym "KINFO-PROC-SIZE-")))
    `(ffi:with-foreign-value (,%count :int)
       (let ((,kinfo-proc-size (ffi:foreign-type-size 'kinfo-proc))
             (,%proc
               (system-funcall
                ("kvm_getprocs" ((:pointer kern-proc :int :pointer) :pointer)
                                ,%kvm ,op ,arg ,%count))))
         (dotimes (i (ffi:foreign-value ,%count :int))
           ,@body
           (setf ,%proc (ffi:pointer+ ,%proc ,kinfo-proc-size)))))))

;;;
;;; Time
;;;

(defun initialize-timeval (%pointer microseconds)
  (declare (type ffi:pointer %pointer)
           (type (integer 0) microseconds))
  (setf (ffi:foreign-structure-member %pointer 'timeval :sec)
        (floor microseconds 1000000))
  (setf (ffi:foreign-structure-member %pointer 'timeval :usec)
        (mod microseconds 1000000)))

;;;
;;; IO multiplexing (Linux)
;;;

#+linux
(defun epoll-create1 (flags)
  (system-funcall ("epoll_create1" ((epoll-flags) :int) flags)))

#+linux
(defun epoll-ctl (epoll-fd operation fd %event)
  (system-funcall ("epoll_ctl" ((:int epoll-op :int :pointer) :int)
                               epoll-fd operation fd %event)))

#+linux
(defmacro with-epoll-wait ((%event epoll-fd max-nb-events timeout) &body body)
  (assert (constantp max-nb-events))
  (let ((%events (gensym "%EVENTS-"))
        (nb-events (gensym "NB-EVENTS-"))
        (i (gensym "I-")))
    `(ffi:with-foreign-value
         (,%events 'epoll-event :count ,max-nb-events)
       (let ((,nb-events
               (system-funcall
                ("epoll_wait" ((:int :pointer :int :int) :int)
                              ,epoll-fd ,%events ,max-nb-events ,timeout))))
         (dotimes (,i ,nb-events)
           (let ((,%event
                   (ffi:pointer+ ,%events
                                 (* ,i (ffi:foreign-type-size 'epoll-event)))))
             ,@body))))))

#+linux
(defun timerfd-create (clock-id flags)
  (system-funcall ("timerfd_create" ((clock-type timerfd-create-flags) :int)
                                    clock-id flags)))

#+linux
(defun timerfd-settime (fd flags interval value)
  (declare (type integer interval value)) ; microseconds
  (flet ((init-timespec (%spec value)
           (multiple-value-bind (seconds microseconds) (floor value 1000000)
             (setf (ffi:foreign-structure-member %spec 'timespec :tv-sec)
                   seconds)
             (setf (ffi:foreign-structure-member %spec 'timespec :tv-nsec)
                   (* microseconds 1000)))))
    (ffi:with-foreign-value (%spec 'itimerspec)
      (init-timespec
       (ffi:foreign-structure-member-pointer %spec 'itimerspec :it-interval)
       interval)
      (init-timespec
       (ffi:foreign-structure-member-pointer %spec 'itimerspec :it-value)
       value)
      (system-funcall
       ("timerfd_settime" ((:int timerfd-settime-flags :pointer :pointer) :int)
                          fd flags %spec (ffi:null-pointer))))))

;;;
;;; IO multiplexing (BSD)
;;;

#+bsd
(defun kqueue ()
  (system-funcall ("kqueue" (() :int))))

#+bsd
(defun kevent (fd %changes nb-changes %events nb-events %timeout)
  (system-funcall
   ("kevent" ((:int :pointer :int :pointer :int :pointer) :int)
             fd %changes nb-changes %events nb-events %timeout)))

;;;
;;; File descriptors
;;;

(deftype fd ()
  '(integer 0))

(defun close-fd (fd)
  (system-funcall ("close" ((:int) :int) fd)))

(defun read-fd (fd %data size)
  (system-funcall ("read" ((:int :pointer size-t) ssize-t) fd %data size)))

(defun write-fd (fd %data size)
  (system-funcall ("write" ((:int :pointer size-t) ssize-t) fd %data size)))

(defmacro fcntl (fd command &rest args)
  `(system-funcall ("fcntl" ((:int fcntl-command ,@(mapcar #'first args)) :int)
                            ,fd ,command ,@(mapcar #'second args))))

(defun fcntl-getfl (fd)
  (fcntl fd :f-getfl))

(defun fcntl-setfl (fd flags)
  (fcntl fd :f-setfl (fcntl-fd-flags flags)))

(defun fcntl-setfl-add-remove-flags (fd added-flags removed-flags)
  (let ((old-bitset (fcntl-getfl fd))
        (added-bitset
          (ffi:encode-foreign-value added-flags 'fcntl-fd-flags))
        (removed-bitset
          (ffi:encode-foreign-value removed-flags 'fcntl-fd-flags)))
    (fcntl-setfl fd (logand (logior old-bitset added-bitset)
                            (lognot removed-bitset)))))

;;;
;;; Pipes
;;;

(defun pipe ()
  (ffi:with-foreign-value (%fds :int :count 2)
    (system-funcall ("pipe" ((:pointer) :int) %fds))
    (values (ffi:foreign-value %fds :int 0)
            (ffi:foreign-value %fds :int 1))))

;;;
;;; Sockets
;;;

(defun socket (domain type protocol)
  (system-funcall ("socket" ((address-family socket-type socket-protocol) :int)
                            domain type protocol)))

(defun setsockopt (socket level option %value value-length)
  (system-funcall
   ("setsockopt"
    ((:int socket-option-level socket-option :pointer socklen-t) :int)
    socket level option %value value-length)))

(defun connect (socket %address address-length)
  (system-funcall ("connect" ((:int :pointer socklen-t) :int)
                             socket %address address-length)))

(defun shutdown (socket shutdown-type)
  (system-funcall ("shutdown" ((:int shutdown-type) :int)
                              socket shutdown-type)))

(defun bind (socket %address address-length)
  (system-funcall ("bind" ((:int :pointer socklen-t) :int)
                          socket %address address-length)))

(defun socket-listen (socket backlog)
  (system-funcall ("listen" ((:int :int) :int) socket backlog)))

(defun accept (socket)
  (ffi:with-foreign-values ((%address 'sockaddr-storage)
                            (%address-length 'socklen-t))
    (let ((address-length (ffi:foreign-type-size 'sockaddr-storage)))
      (ffi:clear-foreign-memory %address address-length)
      (setf (ffi:foreign-value %address-length 'socklen-t) address-length)
      (let ((connection-socket
              (system-funcall ("accept" ((:int :pointer :pointer) :int)
                                        socket %address %address-length))))
        (core:abort-protect
            (let* ((family
                     (ffi:decode-foreign-value
                      (ffi:foreign-structure-member %address 'sockaddr-storage
                                                    :ss-family)
                      'address-family))
                   (address-class
                     (case family
                       (:af-inet 'ipv4-socket-address)
                       (:af-inet6 'ipv6-socket-address)
                       (t (error "unknown socket family ~S" family))))
                   (address
                     (let ((address (make-instance address-class)))
                       (initialize-socket-address-from-sockaddr
                        address %address))))
              (values connection-socket address))
          (close connection-socket))))))

;;;
;;; DNS
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant ni-maxhost 1025)
  (defconstant ni-maxserv 32))

(defmacro with-getaddrinfo ((%info host service
                             &key (%hints (ffi:null-pointer)))
                            &body body)
  (let ((%host (gensym "%HOST-"))
        (%service (gensym "%SERVICE-"))
        (%result (gensym "%RESULT-")))
    `(ffi:with-foreign-strings ((,%host ,host)
                                (,%service ,service))
       (ffi:with-foreign-value (,%result :pointer)
         (system-funcall ("getaddrinfo"
                          ((:pointer :pointer :pointer :pointer) :int)
                          ,%host ,%service ,%hints ,%result)
                         :errorp (complement #'zerop)
                         :error-value-function #'identity
                         :error-description-function 'gai-strerror)
         (unwind-protect
              (do* ((,%info (ffi:foreign-value ,%result :pointer)))
                   ((ffi:null-pointer-p ,%info)
                    nil)
                ,@body
                (setf ,%info
                      (ffi:foreign-structure-member ,%info 'addrinfo
                                                    :ai-next)))
           (system-funcall
            ("freeaddrinfo" ((:pointer) :void)
                            (ffi:foreign-value ,%result :pointer))))))))

(defun getnameinfo (%address address-length flags)
  (ffi:with-foreign-values ((%host :char :count #.ni-maxhost)
                            (%service :char :count #.ni-maxserv))
    (system-funcall ("getnameinfo"
                     ((:pointer socklen-t :pointer socklen-t
                       :pointer socklen-t ni-flags)
                      :int)
                     %address address-length %host #.ni-maxhost
                     %service #.ni-maxserv flags)
                    :errorp (complement #'zerop)
                    :error-value-function #'identity
                    :error-description-function 'gai-strerror)
    (values (ffi:decode-foreign-string %host)
            (ffi:decode-foreign-string %service))))

(defun gai-strerror (value)
  (ffi:decode-foreign-string
   (system-funcall ("gai_strerror" ((:int) :pointer) value))))
