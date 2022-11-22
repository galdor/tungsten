(in-package :check-test)

(deftest check-true ()
  (check-true t)
  (check-true (= 1 1))
  (check-true (member 3 (list 1 2 3))))

(deftest check-false ()
  (check-false nil)
  (check-false (= 1 2))
  (check-false (member 4 (list 1 2 3))))

(deftest check-equal ()
  (check= 1 1)
  (check-eq 'foo 'foo))
