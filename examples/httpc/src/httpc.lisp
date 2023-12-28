(in-package :httpc)

(program:defprogram (httpc)
  :options
  (("m" "method" "method" "the HTTP method of the request"
        :default-value "GET"))
  :arguments
  (("uri" "the uri to send the request to"))
  :function 'main)

(defun main ()
  (format t "Hello world!~%"))
