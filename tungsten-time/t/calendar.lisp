(in-package :time-test)

(deftest week-day ()
  (macrolet ((check-week-days (&rest tests)
               `(progn
                  ,@(mapcar (lambda (test)
                              `(check= ,(car test)
                                       (time:week-day ,@(cadr test))))
                            tests))))
    (check-week-days
     (1 (1900  1  1))
     (3 (1900  2 28))
     (1 (1900 12 31))
     (3 (2020  1  1))
     (6 (2020  2 29))
     (4 (2020 12 31))
     (7 (2023  1  1))
     (1 (2023  1 30))
     (7 (2023  2  5))
     (2 (2023  2 28))
     (7 (2023 12 31)))))

(deftest leap-year-p ()
  (check-true (time:leap-year-p 1600))
  (check-false (time:leap-year-p 1900))
  (check-true (time:leap-year-p 2000))
  (check-true (time:leap-year-p 2008))
  (check-false (time:leap-year-p 2100))
  (check-true (time:leap-year-p 2400)))
