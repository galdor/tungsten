(defpackage :sqlite
  (:use :cl)
  (:export
   #:library-version

   #:sqlite-error
   #:sqlite-error-function
   #:sqlite-error-code
   #:sqlite-error-description

   #:*database*
   #:database-access-mode
   #:open-database
   #:close-database
   #:with-database
   #:do-query-rows
   #:query
   #:query-row))
