(in-package :uuid-test)

(defmacro check-parsing (&rest tests)
  `(progn
     ,@(mapcar (lambda (test)
                 `(check-equalp ,(car test)
                                (uuid:uuid-octets
                                 (uuid:parse ,(cadr test)))))
               tests)))

(defmacro check-serialization (&rest tests)
  `(progn
     ,@(mapcar (lambda (test)
                 `(check-string= ,(car test)
                                 (uuid:serialize (uuid:uuid ,(cadr test)))))
               tests)))

(deftest parse ()
  (check-parsing
   ((core:octet-vector* #xc2 #x32 #xab #x00 #x94 #x14 #x11 #xec
                        #xb3 #xc8 #x9e #x6b #xde #xce #xd8 #x46)
    "c232ab00-9414-11ec-b3c8-9e6bdeced846")
   ((core:octet-vector* #x5d #xf4 #x18 #x81 #x3a #xed #x35 #x15
			                  #x88 #xa7 #x2f #x4a #x81 #x4c #xf0 #x9e)
    "5df41881-3aed-3515-88a7-2f4a814cf09e")
   ((core:octet-vector* #x91 #x91 #x08 #xf7 #x52 #xd1 #x43 #x20
			                  #x9b #xac #xf8 #x47 #xdb #x41 #x48 #xa8)
    "919108f7-52d1-4320-9bac-f847db4148a8")
   ((core:octet-vector* #x2e #xd6 #x65 #x7d #xe9 #x27 #x56 #x8b
			                  #x95 #xe1 #x26 #x65 #xa8 #xae #xa6 #xa2)
    "2ed6657d-e927-568b-95e1-2665a8aea6a2")
   ((core:octet-vector* #x1e #xc9 #x41 #x4c #x23 #x2a #x6b #x00
			                  #xb3 #xc8 #x9e #x6b #xde #xce #xd8 #x46)
    "1ec9414c-232a-6b00-b3c8-9e6bdeced846")
   ((core:octet-vector* #x01 #x7f #x22 #xe2 #x79 #xb0 #x7c #xc3
			                  #x98 #xc4 #xdc #x0c #x0c #x07 #x39 #x8f)
    "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")
   ((core:octet-vector* #x32 #x0c #x3d #x4d #xcc #x00 #x87 #x5b
			                  #x8e #xc9 #x32 #xd5 #xf6 #x91 #x81 #xc0)
    "320c3d4d-cc00-875b-8ec9-32d5f69181c0")))

(deftest serialize ()
  (check-serialization
   ("c232ab00-9414-11ec-b3c8-9e6bdeced846"
    (core:octet-vector* #xc2 #x32 #xab #x00 #x94 #x14 #x11 #xec
                        #xb3 #xc8 #x9e #x6b #xde #xce #xd8 #x46))
   ("5df41881-3aed-3515-88a7-2f4a814cf09e"
    (core:octet-vector* #x5d #xf4 #x18 #x81 #x3a #xed #x35 #x15
			                  #x88 #xa7 #x2f #x4a #x81 #x4c #xf0 #x9e))
   ("919108f7-52d1-4320-9bac-f847db4148a8"
    (core:octet-vector* #x91 #x91 #x08 #xf7 #x52 #xd1 #x43 #x20
			                  #x9b #xac #xf8 #x47 #xdb #x41 #x48 #xa8))
   ("2ed6657d-e927-568b-95e1-2665a8aea6a2"
    (core:octet-vector* #x2e #xd6 #x65 #x7d #xe9 #x27 #x56 #x8b
			                  #x95 #xe1 #x26 #x65 #xa8 #xae #xa6 #xa2))
   ("1ec9414c-232a-6b00-b3c8-9e6bdeced846"
    (core:octet-vector* #x1e #xc9 #x41 #x4c #x23 #x2a #x6b #x00
			                  #xb3 #xc8 #x9e #x6b #xde #xce #xd8 #x46))
   ("017f22e2-79b0-7cc3-98c4-dc0c0c07398f"
    (core:octet-vector* #x01 #x7f #x22 #xe2 #x79 #xb0 #x7c #xc3
			                  #x98 #xc4 #xdc #x0c #x0c #x07 #x39 #x8f))
   ("320c3d4d-cc00-875b-8ec9-32d5f69181c0"
    (core:octet-vector* #x32 #x0c #x3d #x4d #xcc #x00 #x87 #x5b
			                  #x8e #xc9 #x32 #xd5 #xf6 #x91 #x81 #xc0))))
