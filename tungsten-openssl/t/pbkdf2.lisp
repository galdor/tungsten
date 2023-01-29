(in-package :openssl-test)

(deftest pbkdf2 ()
  ;; See RFC 6070 2. PBKDF2 HMAC-SHA1 Test Vectors
  (macrolet ((check-pbkdf2 (key password salt nb-iterations key-length)
               `(check-string=
                 ,key
                 (text:encode-hex-string
                  (openssl:pbkdf2 (text:encode-string ,password)
                                  (text:encode-string ,salt)
                                  :sha1 ,nb-iterations ,key-length)))))
    (check-pbkdf2 "0c60c80f961f0e71f3a9b524af6012062fe037a6"
                  "password" "salt" 1 20)
    (check-pbkdf2 "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957"
                  "password" "salt" 2 20)
    (check-pbkdf2 "4b007901b765489abead49d926f721d065a429c1"
                  "password" "salt" 4096 20)
    (check-pbkdf2 "eefe3d61cd4da4e4e9945b3d6ba2158c2634e984"
                  "password" "salt" 16777216 20)
    (check-pbkdf2 "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038"
                  "passwordPASSWORDpassword"
                  "saltSALTsaltSALTsaltSALTsaltSALTsalt" 4096 25)
    (check-pbkdf2 "56fa6aa75548099dcc37d7f03425e0c3"
                  (format nil "pass~Cword" (code-char 0))
                  (format nil "sa~Clt" (code-char 0))
                  4096 16)))
