(in-package :time-test)

(deftest datetime-days-encoding ()
  ;; Tests values generated using GNU date.
  ;; For example:
  ;; date -u -d '2000-03-01 +365 days' +%F
  (macrolet ((check-days (&rest tests)
               `(progn
                  ,@(mapcar
                     (lambda (test)
                       `(progn
                          (check-equalp ',(car test)
                                        (multiple-value-list
                                         (time::decode-datetime-days
                                          ,(cadr test))))
                          (check= ,(cadr test)
                                  (time::encode-datetime-days
                                   ,@(car test)))))
                     tests))))
    (check-days
     ((1600  6  6) -146000)
     ((1600  6  7) -145999)
     ((1900  3 26)  -36500)
     ((1900  3 27)  -36499)
     ((1996  3  2)   -1460)
     ((1996  3  3)   -1459)
     ((1999  3  2)    -365)
     ((1999  3  3)    -364)
     ((2000  2 29)      -1)
     ((2000  3  1)       0)
     ((2000  3  2)       1)
     ((2001  2 28)     364)
     ((2001  3  1)     365)
     ((2004  2 28)    1459)
     ((2004  2 29)    1460)
     ((2100  2  4)   36499)
     ((2100  2  5)   36500)
     ((2399 11 24)  145999)
     ((2399 11 25)  146000))))

(deftest datetime-seconds-encoding ()
  (macrolet ((check-seconds (&rest tests)
               `(progn
                  ,@(mapcar
                     (lambda (test)
                       `(progn
                          (check-equalp ',(car test)
                                        (multiple-value-list
                                         (time::decode-datetime-seconds
                                          ,(cadr test))))
                          (check= ,(cadr test)
                                  (time::encode-datetime-seconds
                                   ,@(car test)))))
                     tests))))
    (check-seconds
     (( 0  0  0) 0)
     ((12  0  0) 43200)
     ((23 59 59) 86399))))

(deftest datetime-unix-timestamps ()
  (macrolet ((check-timestamp (&rest tests)
               `(progn
                  ,@(mapcar
                     (lambda (test)
                       (let ((datetime (gensym "DATETIME-")))
                         `(let ((,datetime
                                  (time:make-datetime-from-unix-timestamp
                                   ,(cadr test) ,@(cddr test))))
                            (check-equalp
                             ',(car test)
                             (multiple-value-list
                              (time:decode-datetime ,datetime)))
                            (check=
                             ,(cadr test)
                             (time:datetime-unix-timestamp
                              ,datetime ,@(cddr test))))))
                     tests))))
    (check-timestamp
     ((1600  6  7 12  0  0         0) -11662401600)
     ((1970  1  1  0  0  0         0)            0)
     ((1970  1  1  0  0  1 234000000)            1234       :unit :millisecond)
     ((1970  1  1  0  0  1 234567000)            1234567    :unit :microsecond)
     ((1970  1  1  0  0  1 234567890)            1234567890 :unit :nanosecond)
     ((2000  2 29 23 59 59         0)    951868799)
     ((2000  3  1 10 20 30         0)    951906030)
     ((2023  1 31 14 10 40 975297000)   1675174240975297    :unit :microsecond)
     ((2100  2  5  0  0  1         0)   4105468801))))
