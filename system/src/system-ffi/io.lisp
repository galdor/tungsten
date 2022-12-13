(in-package :system-ffi)

(defconstant ni-maxhost 1025)
(defconstant ni-maxserv 32)

(defun close-fd (fd)
  (system-funcall ("close" ((:int) :int) fd)))

(defun read-fd (fd %data size)
  (system-funcall ("read" ((:int :pointer size-t) ssize-t) fd %data size)))

(defun write-fd (fd %data size)
  (system-funcall ("write" ((:int :pointer size-t) ssize-t) fd %data size)))

(defun socket (domain type protocol)
  (system-funcall ("socket" ((address-family socket-type socket-protocol) :int)
                            domain type protocol)))

(defun connect (fd %address address-length)
  (system-funcall ("connect" ((:int :pointer socklen-t) :int)
                             fd %address address-length)))

(defun shutdown (fd shutdown-type)
  (system-funcall ("shutdown" ((:int shutdown-type) :int) fd shutdown-type)))

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
