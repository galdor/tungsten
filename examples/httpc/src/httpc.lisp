(in-package :httpc)

(program:defprogram (httpc)
  :options
  (("m" "method" "method" "the HTTP method of the request"
        :default-value "GET"))
  :arguments
  (("uri" "the uri to send the request to"))
  :function 'main)

(program:defprogram (test)
  :options
  (("a" nil "value" "a option")
   ("b" nil nil "b flag"))
  :commands
  (("foo"
    :options
    (("c" nil "value" "c option")
     ("d" nil nil "d flag"))
    :arguments
    (("arg" "blabla" :trailing t))
    :function 'main)
   ("bar"
    :options
    ()
    :arguments
    ()
    :function 'main)))

(defun main ()
  (let ((method (program:option-value "method"))
        (uri (uri:parse (program:argument-value "uri"))))
    (format t "sending ~A request to ~A~%" method uri)))
