(in-package :mime-test)

(defmacro check-encode-quoted-printable (&rest checks)
  `(progn
     ,@(mapcar (lambda (check)
                 `(check-string=
                   ,(car check)
                   (with-output-to-string (stream)
                     (mime:encode-quoted-printable
                      ,(cadr check) stream ,@(cddr check)))))
               checks)))

(deftest encode-quoted-printable ()
  (check-encode-quoted-printable
   ("" "")
   ("foo" "foo")
   (#.(format nil "foo~C~Cbar~C~C~C~Cbaz~C~C"
              #\Return #\Newline
              #\Return #\Newline
              #\Return #\Newline
              #\Return #\Newline)
      #.(format nil "foo~Cbar~C~Cbaz~C"
                #\Newline #\Newline #\Newline #\Newline))
   ("R=C3=A9publique Fran=C3=A7aise" "République Française")
   ("  hello  world=20=20" "  hello  world  ")
   (#.(format nil "foo =~C~Cbar =~C~Cbaz"
              #\Return #\Newline
              #\Return #\Newline)
    "foo bar baz" :max-line-length 4)))
