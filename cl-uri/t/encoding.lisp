(in-package :uri-test)

(deftest percent-decode ()
  (check-string= "" (uri:percent-decode ""))
  (check-string= "foo" (uri:percent-decode "foo"))
  (check-string= " " (uri:percent-decode "%20"))
  (check-string= "abc" (uri:percent-decode "%61%62%63"))
  (check-string= "aà€𝄞" (uri:percent-decode "%61%c3%a0%e2%82%AC%f0%9d%84%9E"))
  (check-signals error (uri:percent-decode "%")) ; TODO error type
  (check-signals error (uri:percent-decode "%6")) ; TODO error type
  (check-signals error (uri:percent-decode "%6g")) ; TODO error type
  (check-signals error (uri:percent-decode "%,1"))) ; TODO error type

(deftest percent-encode ()
  (flet ((validp (c)
           (or (char<= #\a c #\z)
               (char= c #\/)
               (char= c #\?))))
    (check-string= "" (uri:percent-encode "" #'validp))
    (check-string= "abc" (uri:percent-encode "abc" #'validp))
    (check-string= "/a?b?%23?c/" (uri:percent-encode "/a?b?#?c/" #'validp))
    (check-string= "%C3%A9t%C3%A9" (uri:percent-encode "été" #'validp))
    (check-string= "a%C3%A0%E2%82%AC%F0%9D%84%9E"
                   (uri:percent-encode "aà€𝄞" #'validp))))
