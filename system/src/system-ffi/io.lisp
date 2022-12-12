(in-package :system-ffi)

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

(defmacro with-getaddrinfo ((%info node service
                             &key (%hints (ffi:null-pointer)))
                            &body body)
  (let ((%node (gensym "%NODE-"))
        (%service (gensym "%SERVICE-"))
        (%result (gensym "%RESULT-")))
    `(ffi:with-foreign-strings ((,%node ,node)
                                (,%service ,service))
       (ffi:with-foreign-value (,%result :pointer)
         (system-funcall ("getaddrinfo"
                          ((:pointer :pointer :pointer :pointer) :int)
                          ,%node ,%service ,%hints ,%result)
                         :errorp (complement #'zerop)
                         :error-value-function #'identity
                         :error-description-function 'gai-strinfo)
         (unwind-protect
              (do* ((,%info (ffi:foreign-value ,%result :pointer)))
                   ((ffi:null-pointer-p ,%info)
                    nil)
                ,@body
                (setf ,%info (ffi:struct-member ,%info 'addrinfo :ai-next)))
           (system-funcall
            ("freeaddrinfo" ((:pointer) :void)
                            (ffi:foreign-value ,%result :pointer))))))))

(defun gai-strinfo (value)
  (ffi:decode-foreign-string
   (system-funcall ("gai_strerror" ((:int) :pointer) value))))
