(in-package :httpc)

(program:defprogram (httpc)
  :options
  (("m" "method" "method" "the HTTP method of the request"
        :default-value "GET"))
  :arguments
  (("uri" "the URI to send the request to"))
  :function 'main)

(program:defprogram (test)
  :options
  (("a" "a-option" "value" "a option")
   ("b" nil nil "b flag"))
  :commands
  (("foo"
    "do foo"
    :options
    (("c" "c-flag" "value" "c option")
     (nil "d-flag" nil "d flag"))
    :arguments
    (("arg-1" "blabla 1")
     ("arg-2" "blabla 2" :trailing t))
    :function 'main)
   ("bar"
    "do bar"
    :options
    ()
    :arguments
    ()
    :function 'main)))

(defun main ()
  (let ((method (program:option-value "method"))
        (uri (uri:parse (program:argument-value "uri"))))
    (format t "sending ~A request to ~A~%" method uri)))
