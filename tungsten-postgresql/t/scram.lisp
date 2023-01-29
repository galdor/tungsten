(in-package :postgresql-test)

(deftest scram-client-first-message ()
  (macrolet ((check-message (expected-string username nonce)
               `(check-string= ,expected-string
                               (postgresql::scram-client-first-message
                                ,username ,nonce))))
    (check-message "n,,n=,r=bar" nil "bar")
    (check-message "n,,n=foo,r=bar" "foo" "bar")
    (check-message "n,,n=ab=3Dcd,r=bar" "ab=cd" "bar")
    (check-message "n,,n==3Dab=2Ccd=3D,r=bar" "=ab,cd=" "bar")))

(deftest scram-client-final-message ()
  ;; RFC 7677 3. SCRAM-SHA-256 and SCRAM-SHA-256-PLUS
  (let* ((username "user")
         (password "pencil")
         (client-nonce "rOprNGfwEbeRWgbNEkqO")
         (client-first-message
           (format nil "n,,n=~A,r=~A" username client-nonce))
         (server-first-message
           (concatenate 'string
                        "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"
                        ",s=W22ZaJ0SNY7soEsUEjb6gQ=="
                        ",i=4096")))
    (check-string=
     #.(concatenate 'string
                    "c=biws"
                    ",r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"
                    ",p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=")
     (postgresql::scram-client-final-message client-first-message
                                             server-first-message
                                             password
                                             client-nonce))))
