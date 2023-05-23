(in-package :sqlite-test)

(defmacro check-query-row (&rest forms)
  `(sqlite:with-database ("test" :in-memory t)
     ,@(mapcar
        (lambda (form)
          (destructuring-bind (expected-row query &optional parameters)
              form
            `(test:check-equalp ,expected-row
                                (sqlite:query-row ,query ,parameters))))
        forms)))

(deftest value-mapping/null ()
  (check-query-row
   (#(nil) "SELECT NULL")
   (#(nil) "SELECT ?" #(nil))))

(deftest value-mapping/integer ()
  (check-query-row
   (#(1 2 3) "SELECT 1, 2, 3")
   (#(42) "SELECT ?" #(42))
   (#(-9223372036854775808 9223372036854775807)
     "SELECT -9223372036854775808, 9223372036854775807")
   (#(-9223372036854775808 9223372036854775807)
     "SELECT ?, ?" #(-9223372036854775808 9223372036854775807))))

(deftest value-mapping/float ()
  (check-query-row
   (#(0.0d0 3.14159d0 -1.23d45) "SELECT 0.0, 3.14159, -1.23e45")
   (#(0.0d0 3.14159d0 -1.23d45) "SELECT ?, ?, ?" #(0.0d0 3.14159d0 -1.23d45))))

(deftest value-mapping/text ()
  (check-query-row
   (#("hello" "" "1€") "SELECT 'hello', '', '1€'")
   (#("hello" "" "1€") "SELECT ?, ?, ?" #("hello" "" "1€"))))

(deftest value-mapping/blob ()
  (check-query-row
   ((vector (core:octet-vector* 0 1 2)) "SELECT x'000102'")
   ((vector (core:octet-vector* 0 1 2))
    "SELECT ?" (vector (core:octet-vector* 0 1 2)))
   ((vector (core:octet-vector*)) "SELECT ?" (vector (core:octet-vector*)))))
