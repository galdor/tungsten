(in-package :http)

(defvar *routers* (make-hash-table))

(defvar *router* nil)

(defvar *request* nil)
(defvar *request-context* nil)

(define-condition unknown-router (error)
  ((name
    :type symbol
    :initarg :name
    :reader unknown-router-name))
  (:report
   (lambda (condition stream)
     (with-slots (name) condition
       (format stream "Unknown HTTP router ~S." name)))))

(defclass router ()
  ((name
    :type symbol
    :initarg :name
    :accessor router-name)
   (mutex
    :type system:mutex
    :initform (system:make-mutex :name "http-router"))
   (routes
    :type list
    :initarg :routes
    :initform nil
    :accessor router-routes)))

(defclass request-context ()
  ((client-address
    :type system:socket-address
    :initarg :client-address
    :reader request-context-client-address)
   (route
    :type route
    :initarg :route
    :reader request-context-route)
   (path-variables
    :type list
    :initarg :path-variables
    :initform nil
    :reader request-context-path-variables)))

(defmethod print-object ((router router) stream)
  (print-unreadable-object (router stream :type t)
    (princ (router-name router) stream)))

(defmacro defrouter (name)
  (let ((name-var (gensym "NAME-"))
        (router (gensym "ROUTER-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,name-var ',name)
              (,router (make-instance 'router :name ,name-var)))
         (setf (gethash ,name-var *routers*) ,router)))))

(defmacro in-router (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *router* (find-router ',name))))

(defun find-router (name)
  (etypecase name
    (symbol
     (or (gethash name *routers*)
         (error 'unknown-router :name name)))
    (router
     name)))

(defun delete-router (name)
  (unless (remhash name *routers*)
    (error 'unknown-router :name name)))

(defun router-request-handler (router)
  (declare (type router router))
  (lambda (request connection)
    (router-handle-request router request connection)))

(defun router-handle-request (router request connection)
  (declare (type router router)
           (type request request)
           (type connection connection))
  (multiple-value-bind (route path-variables)
      (router-find-matching-route router request)
    (if route
        (let* ((client-address (connection-address connection))
               (context (make-instance 'request-context
                                       :client-address client-address
                                       :route route
                                       :path-variables path-variables)))
          (funcall (route-request-handler route) request context))
        (make-plain-text-response 404 "Route not found."))))

(defun router-find-matching-route (router request)
  (with-slots (mutex routes) router
    (system:with-mutex (mutex)
      (mapc (lambda (route)
              (multiple-value-bind (matchp path-variables)
                  (match-route route request)
                (when matchp
                  (return-from router-find-matching-route
                    (values route path-variables)))))
            routes)
      (values nil nil))))

(defmacro defroute (name (method path) &body body)
  (let ((function (gensym "FUNCTION-"))
        (route (gensym "ROUTE-")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let* ((,function
                (lambda (request context)
                  (declare (type request request)
                           (type request-context context))
                  (let ((*request* request)
                        (*request-context* context))
                    ,@body)))
              (,route (make-instance 'route
                                     :name ',name
                                     ,@(when method (list :method method))
                                     :path (parse-route-path ,path)
                                     :request-handler ,function)))
         (with-slots (mutex routes) *router*
           (system:with-mutex (mutex)
             (push ,route routes)))))))

(defun delete-route (name &optional (router *router*))
  (with-slots (mutex routes) (find-router router)
    (system:with-mutex (mutex)
      (setf routes (delete name routes :key 'route-name)))
    nil))

(defun path-variables ()
  (request-context-path-variables *request-context*))

(defun path-variable (name &optional default)
  (declare (type symbol name))
  (with-slots (path-variables) *request-context*
    (or (cdr (assoc name path-variables)) default)))

(defun route ()
  (request-context-route *request-context*))

(defun client-address ()
  (request-context-client-address *request-context*))
