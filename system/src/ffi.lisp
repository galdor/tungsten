(in-package :system)

;;;
;;; Time
;;;

(defun initialize-timeval (%pointer microseconds)
  (declare (type ffi:pointer %pointer)
           (type (integer 0) microseconds))
  (setf (ffi:struct-member %pointer 'timeval :sec)
        (floor microseconds 1000000))
  (setf (ffi:struct-member %pointer 'timeval :usec)
        (mod microseconds 1000000)))

;;;
;;; IO multiplexing
;;;

#+linux
(defun epoll-create1 (flags)
  (system-funcall ("epoll_create1" ((epoll-flags) :int) flags)))

#+linux
(defun epoll-ctl (epoll-fd operation fd %event)
  (system-funcall ("epoll_ctl" ((:int epoll-op :int :pointer) :int)
                               epoll-fd operation fd %event)))

;;;
;;; File descriptors
;;;

(defun close-fd (fd)
  (system-funcall ("close" ((:int) :int) fd)))

(defun read-fd (fd %data size)
  (system-funcall ("read" ((:int :pointer size-t) ssize-t) fd %data size)))

(defun write-fd (fd %data size)
  (system-funcall ("write" ((:int :pointer size-t) ssize-t) fd %data size)))

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
                      (ffi:struct-member %address 'sockaddr-storage :ss-family)
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
                (setf ,%info (ffi:struct-member ,%info 'addrinfo :ai-next)))
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
