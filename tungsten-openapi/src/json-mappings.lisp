(in-package :openapi)

;;; Reference: https://spec.openapis.org/oas/v3.1.0.html

(json:register-mapping-class 'reference 'reference-json-mapping)
(json:register-mapping-class 'responses 'responses-json-mapping)

(defclass reference-json-mapping (json:object-mapping)
  ((type
    :type (or symbol null)
    :initarg :type
    :initform nil
    :reader reference-json-mapping-type))
  (:default-initargs
   :members
   '(("$ref" ref (:string))
     ("summary" summary (:string))
     ("description" description (:string)))
   :required
   '("$ref")))

(defmethod json:validate-value :around (value (mapping reference-json-mapping))
  (let* ((value (call-next-method value mapping))
         (reference-type (reference-json-mapping-type mapping))
         (ref-member (assoc 'ref value))
         (ref (cdr ref-member)))
    (when ref
      (handler-case
          (let* ((uri (uri:parse ref))
                 (fragment (uri:uri-fragment uri))
                 (pointer (when fragment (json:parse-pointer fragment))))
            (cond
              ((or (uri:uri-host uri) (uri:uri-path uri))
               (json:add-mapping-error value "unsupported external reference")
               value)
              ((null fragment)
               (json:add-mapping-error value "missing reference URI fragment")
               value)
              ((and (= (length pointer) 3)
                    (string= (first pointer) "components"))
               (let ((type (core:string-case (second pointer)
                             ("callbacks" 'callback)
                             ("examples" 'example)
                             ("headers" 'header)
                             ("links" 'link)
                             ("parameters" 'parameter)
                             ("pathItems" 'path-item)
                             ("requestBodies" 'request-body)
                             ("responses" 'response)
                             ("schemas" 'schema)
                             ("securitySchemes" 'security-scheme)
                             (t (json:add-mapping-error
                                 value "unsupported reference to unknown ~
                                      component type ~S"
                                 (second pointer)))))
                     (name (third pointer)))
                 (when (and reference-type (not (eq type reference-type)))
                   (json:add-mapping-error
                    value "reference does not target a component of type ~A"
                    reference-type))
                 (rplacd ref-member (list type name))
                 value))
              (t
               (json:add-mapping-error
                value "reference does not point to a component")
               value)))
        (uri:uri-parse-error (condition)
          (json:add-mapping-error value "string is not a valid URI: ~A"
                                  condition)
          value)
        (json:pointer-parse-error (condition)
          (json:add-mapping-error
           value "~?"
           (json:pointer-parse-error-format-control condition)
           (json:pointer-parse-error-format-arguments condition))
          value)))))

(defmethod json:generate-value (value (mapping reference-json-mapping))
  (declare (ignore mapping))
  (uri:serialize value))

(defclass responses-json-mapping (json:object-mapping)
  ()
  (:default-initargs
   :members
   '(("default" :default (:or :mappings ((reference :type response) response))))
   :value
   '(:or :mappings ((reference :type response) response))))

(defmethod json:validate-value :around (value (mapping responses-json-mapping))
  (let ((value (call-next-method value mapping)))
    (dolist (member value)
      (let ((key (car member)))
        (unless (eq key :default)
          (rplaca member
                  (cond
                    ((every #'digit-char-p key)
                     (parse-integer key))
                    (t
                     (json:add-mapping-error
                      value "invalid response status code ~S" key)
                     key))))))
    value))

(defmethod json:generate-value :around (value (mapping reference-json-mapping))
  (declare (ignore mapping))
  (dolist (member value)
    (when (integerp (car member))
      (rplaca member (princ-to-string (car member)))))
  (call-next-method value))

(json:define-mapping document
  :object
  :members
  (("openapi" openapi (:string))
   ("info" info info)
   ("jsonSchemaDialect" json-dialect (:string))
   ("servers" servers (:array :element server))
   ("paths" paths (:object :value path-item))
   ("webhooks"
    webhooks
    (:object :value (:or :mappings ((reference :type webhook) path-item))))
   ("components" components components)
   ("security" security (:array :element security-requirement))
   ("tags" tags (:array :element tag))
   ("externalDocs" external-docs external-documentation))
  :required
  ("openapi" "info"))

(json:define-mapping info
  :object
  :members
  (("title" title (:string))
   ("summary" summary (:string))
   ("description" description (:string))
   ("termsOfService" terms-of-service (uri:uri))
   ("contact" contact contact)
   ("license" license license)
   ("version" version (:string)))
  :required
  ("title" "version"))

(json:define-mapping contact
  :object
  :members
  (("name" name (:string))
   ("url" url (uri:uri))
   ("email" email (:string))))

(json:define-mapping license
  :object
  :members
  (("name" name (:string))
   ("identifier" identifier (:string))
   ("url" url (uri:uri)))
  :required
  ("name"))

(json:define-mapping server
  :object
  :members
  (("url" url (uri:uri))
   ("description" description (:string))
   ("variables" variables (:object :value server-variable)))
  :required
  ("url"))

(json:define-mapping server-variable
  :object
  :members
  (("enum" enum (:array :element (:string) :min-length 1))
   ("default" default (:string))
   ("description" description (:string)))
  :required
  ("default"))

(json:define-mapping callback
  :object
  :value (:or :mappings ((reference :type path-item) path-item)))

(json:define-mapping path-item
  :object
  :members
  (("summary" summary (:string))
   ("description" description (:string))
   ("get" get operation)
   ("put" put operation)
   ("post" post operation)
   ("delete" delete operation)
   ("options" options operation)
   ("head" head operation)
   ("patch" patch operation)
   ("trace" trace operation)
   ("servers" servers (:array :element server))
   ("parameters"
    parameters
    (:array :element (:or :mappings ((reference :type parameter)
                                     parameter))))))

(json:define-mapping operation
  :object
  :members
  (("tags" tags (:array :element (:string)))
   ("summary" summary (:string))
   ("description" description (:string))
   ("externalDocs" external-docs external-documentation)
   ("operationId" operation-id (:string))
   ("parameters"
    parameters
    (:array :element (:or :mappings ((reference :type parameter) parameter))))
   ("requestBody"
    request-body (:or :mappings ((reference :type request-body) request-body)))
   ("responses" responses (responses))
   ("callbacks"
    callbacks
    (:object :value (:or :mappings ((reference :type callback) callback))))
   ("deprecated" deprecated (:boolean))
   ("security" security (:array :element security-requirement))
   ("servers" servers (:array :element server))))

(json:define-mapping parameter
  :object
  :members
  (("name" name (:string))
   ("in" in (:string :value (("query" :query)
                             ("header" :header)
                             ("path" :path)
                             ("cookie" :cookie))))
   ("description" description (:string))
   ("required" required (:boolean))
   ("deprecated" deprecated (:boolean))
   ("allowEmptyValue" allow-empty-value (:boolean))
   ("style" style (:string :value (("matrix" :matrix)
                                   ("label" :label)
                                   ("form" :form)
                                   ("simple" :simple)
                                   ("spaceDelimited" :space-delimited)
                                   ("pipeDelimited" :pipe-delimited)
                                   ("deepObject" :deep-object))))
   ("explode" explode (:boolean))
   ("allowReserved" allow-reserved (:boolean))
   ("schema" schema (:or :mappings ((reference :type schema) schema)))
   ("example" example (:any))
   ("examples"
    examples (:object :value (:or :mappings ((reference :type example)
                                             example)))))
  :required
  ("name" "in"))

(json:define-mapping components
  :object
  :members
  (("schemas" schemas (:object :value (:or :mappings ((reference :type schema)
                                                      schema))))
   ("responses"
    responses (:object :value (:or :mappings ((reference :type response)
                                              response))))
   ("parameters"
    parameters
    (:object :value (:or :mappings ((reference :type parameter) parameter))))
   ("examples"
    examples (:object :value (:or :mappings ((reference :type example)
                                             example))))
   ("requestBodies"
    request-bodies
    (:object :value (:or :mappings ((reference :type request-body)
                                    request-body))))
   ("headers"
    headers (:object :value (:or :mappings ((reference :type header)
                                            header))))
   ("securitySchemes"
    security-schemes
    (:object :value (:or :mappings ((reference :type security-scheme)
                                    security-scheme))))
   ("links"
    links (:object :value (:or :mappings ((reference :type link) link))))
   ("callbacks"
    callbacks
    (:object :value (:or :mappings ((reference :type callback) callback))))
   ("pathItems"
    path-items
    (:object :value (:or :mappings ((reference :type path-item) path-item))))))

(json:define-mapping schema
  :object
  :members
  (("title" title (:string))
   ("multipleOf" multiple-of (:integer :min 1))
   ("maximum" maximum (:number))
   ("exclusiveMaximum" exclusive-maximum (:boolean))
   ("minimum" minimum (:number))
   ("exclusiveMinimum" exclusive-minimum (:boolean))
   ("maxLength" max-length (:integer :min 0))
   ("minLength" min-length (:integer :min 0))
   ("pattern" pattern (:string))
   ("maxItems" max-items (:integer :min 0))
   ("minItems" min-items (:integer :min 0))
   ("uniqueItems" unique-items (:boolean))
   ("maxProperties" max-properties (:integer :min 0))
   ("minProperties" min-properties (:integer :min 0))
   ("required" required (:array :element (:string)))
   ("enum" enum (:array))
   ("type"
    type
    (:string :value (("object" :object)
                     ("string" :string)
                     ("integer" :integer)
                     ("number" :number)
                     ("array" :array)
                     ("boolean" :boolean))))
   ("allOf" all-of (:array :element (:or :mappings ((reference :type schema)
                                                    schema))))
   ("oneOf" one-of (:array :element (:or :mappings ((reference :type schema)
                                                    schema))))
   ("anyOf" any-of (:array :element (:or :mappings ((reference :type schema)
                                                    schema))))
   ("not" not schema)
   ("items" items schema)
   ("properties"
    properties (:object :value (:or :mappings ((reference :type schema)
                                               schema))))
   ("additionalProperties"
    additional-properties (:or :mappings ((:boolean)
                                          (reference :type schema)
                                          schema)))
   ("description" description (:string))
   ("format" format (:string))
   ("default" default (:any))
   ("nullable" nullable (:boolean))
   ("discriminator" discriminator discriminator)
   ("xml" xml xml)
   ("externalDocs" external-docs external-documentation)
   ("example" example (:any))))

(json:define-mapping discriminator
  :object
  :members
  (("propertyName" property-name (:string))
   ("mapping" mapping (:object :value (:string))))
  :required
  ("propertyName"))

(json:define-mapping xml
  :object
  :members
  (("name" name (:string))
   ("namespace" namespace (:string))
   ("prefix" prefix (:string))
   ("attribute" attribute (:boolean))
   ("wrapped" wrapped (:boolean))))

(json:define-mapping response
  :object
  :members
  (("description" description (:string))
   ("headers"
    headers (:object :value (:or :mappings ((reference :type header) header))))
   ("content" content (:object :name (mime:media-range) :value media-type))
   ("links"
    links (:object :value (:or :mappings ((reference :type link) link)))))
  :required
  ("description"))

(json:define-mapping example
  :object
  :members
  (("summary" summary (:string))
   ("description" description (:string))
   ("value" value (:any))
   ("externalValue" external-value (:string))))

(json:define-mapping request-body
  :object
  :members
  (("description" description (:string))
   ("content" content (:object :name (mime:media-range) :value media-type))
   ("required" required (:boolean))))

(json:define-mapping media-type
  :object
  :members
  (("schema" schema (:or :mappings ((reference :type schema) schema)))
   ("example" example (:any))
   ("examples"
    examples
    (:object :value (:or :mappings ((reference :type example) example))))
   ("encoding" encoding (:object :value 'encoding))))

(json:define-mapping encoding
  :object
  :members
  (("contentType" content-type (:string))
   ("headers"
    headers (:object :value (:or :mappings ((reference :type header) header))))
   ("style" style (:string))
   ("explode" explode (:boolean))
   ("allowReserved" allow-reserved (:boolean))))

(json:define-mapping header
  :object
  :members
  (("description" description (:string))
   ("required" required (:boolean))
   ("deprecated" deprecated (:boolean))
   ("allowEmptyValue" allow-empty-value (:boolean))
   ("style" style (:string))
   ("explode" explode (:boolean))
   ("allowReserved" allow-reserved (:boolean))
   ("schema" schema (:or :mappings ((reference :type schema) schema)))
   ("example" example (:any))
   ("examples"
    examples (:object :value (:or :mappings ((reference :type example)
                                             example))))))

(json:define-mapping security-scheme
  :object
  :members
  (("type" type (:string))
   ("description" description (:string))
   ("name" name (:string))
   ("in" in (:string :value ("query" "header" "cookie")))
   ("scheme" scheme (:string))
   ("bearerFormat" bearer-format (:string))
   ("flows" flows oauth-flows)
   ("openIdConnectUrl" openid-connect-url (uri:uri)))
  :required
  ("type" "name" "in" "scheme" "flows" "openIdConnectUrl"))

(json:define-mapping oauth-flows
  :object
  :members
  (("implicit" implicit oauth-flow)
   ("password" password oauth-flow)
   ("clientCredentials" client-credentials oauth-flow)
   ("authorizationCode" authorization-code oauth-flow)))

(json:define-mapping oauth-flow
  :object
  :members
  (("authorizationUrl" authorization-url (uri:uri))
   ("tokenUrl" token-url (uri:uri))
   ("refreshUrl" refresh-url (uri:uri))
   ("scopes" scopes (:object :value (:string))))
  :required
  ("authorizationUrl" "tokenUrl" "scopes"))

(json:define-mapping link
  :object
  :members
  (("operationRef" operation-ref (:string))
   ("operationId" operation-id (:string))
   ("parameters" parameters (:object))
   ("requestBody" request-body (:any))
   ("description" description (:string))
   ("server" server server)))

(json:define-mapping security-requirement
  :object
  :value (:array :element (:string)))

(json:define-mapping tag
  :object
  :members
  (("name" name (:string))
   ("description" description (:string))
   ("externalDocs" external-docs external-documentation))
  :required
  ("name"))

(json:define-mapping external-documentation
  :object
  :members
  (("description" description (:string))
   ("url" url (uri:uri)))
  :required
  ("description"))
