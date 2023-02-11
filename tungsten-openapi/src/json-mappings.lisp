(in-package :openapi)

;;; Reference: https://spec.openapis.org/oas/v3.1.0.html

(json:define-mapping openapi
  :object
  :members
  (("openapi" openapi (:string))
   ("info" info info)
   ("jsonSchemaDialect" json-dialect (:string))
   ("servers" servers (:array :element server))
   ("paths" paths (:object :value path-item))
   ("webhooks" webhooks (:object :value (:or :mappings (path-item reference))))
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
   ("termsOfService" terms-of-service (:string))
   ("contact" contact contact)
   ("license" license license)
   ("version" version (:string)))
  :required
  ("title" "version"))

(json:define-mapping contact
  :object
  :members
  (("name" name (:string))
   ("url" url (:string))
   ("email" email (:string))))

(json:define-mapping license
  :object
  :members
  (("name" name (:string))
   ("identifier" identifier (:string))
   ("url" url (:string)))
  :required
  ("name"))

(json:define-mapping server
  :object
  :members
  (("url" url (:string))
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

(json:define-mapping path-item
  :object
  :members
  (("$ref" ref (:string))
   ("summary" summary (:string))
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
   ("parameters" parameters
                 (:array :element (:or :mappings (parameter reference))))))

(json:define-mapping operation
  :object
  :members
  (("tags" tags (:array :element (:string)))
   ("summary" summary (:string))
   ("description" description (:string))
   ("externalDocs" external-docs external-documentation)
   ("operationId" operation-id (:string))
   ("parameters"
    parameters (:array :element (:or :mappings (parameter reference))))
   ("requestBody" request-body (:or :mappings (request-body reference)))
   ("responses" responses responses)
   ("callbacks"
    callbacks (:object :value (:or :mappings (callback reference))))
   ("deprecated" deprecated (:boolean))
   ("security" security (:array :element security-requirement))
   ("servers" servers (:array :element server))))

(json:define-mapping parameter
  :object
  :members
  (("name" name (:string))
   ("in" in (:string :value ("query" "header" "path" "cookie")))
   ("description" description (:string))
   ("required" required (:boolean))
   ("deprecated" deprecated (:boolean))
   ("allowEmptyValue" allow-empty-value (:boolean))
   ("style" style (:string))
   ("explode" explode (:boolean))
   ("allowReserved" allow-reserved (:boolean))
   ("schema" schema schema)
   ("example" example (:any))
   ("examples" examples (:object :value (:or :mappings (example reference)))))
  :required
  ("name" "in"))

(json:define-mapping reference
  :object
  :members
  (("$ref" ref (:string))
   ("summary" summary (:string))
   ("description" description (:string)))
  :required
  ("$ref"))

(json:define-mapping components
  :object
  :members
  (("schemas" schemas (:object :value schema))
   ("responses"
    responses (:object :value (:or :mappings (response reference))))
   ("parameters"
    parameters (:object :value (:or :mappings (parameter reference))))
   ("examples"
    examples (:object :value (:or :mappings (example reference))))
   ("requestBodies"
    request-bodies (:object :value (:or :mappings (request-body reference))))
   ("headers"
    headers (:object :value (:or :mappings (header reference))))
   ("securitySchemes"
    security-schemes
    (:object :value (:or :mappings (security-scheme reference))))
   ("links" links (:object :value (:or :mappings (link reference))))
   ("callbacks"
    callbacks (:object :value (:or :mappings (callback reference))))
   ("pathItems"
    path-items (:object :value (:or :mappings (path-item reference))))))

(json:define-mapping schema
  :object
  :members
  (("$ref" ref (:string))
   ("title" title (:string))
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
    (:string :value ("object" "string" "integer" "number" "array" "boolean")))
   ("allOf" all-of (:array :element schema))
   ("oneOf" one-of (:array :element schema))
   ("anyOf" any-of (:array :element schema))
   ("not" not schema)
   ("items" items schema)
   ("properties" properties (:object :value schema))
   ("additionalProperties"
    additional-properties (:or :mappings ((:boolean) schema)))
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

(json:define-mapping responses
  :object
  :members
  (("default" default (:or :mappings (response reference))))
  :value (:or :mappings (response reference)))

(json:define-mapping response
  :object
  :members
  (("description" description (:string))
   ("headers" headers (:object :value (:or :mappings (header reference))))
   ("content" content (:object :value media-type))
   ("links" links (:object :value (:or :mappings (link reference)))))
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
   ("content" content (:object :value media-type))
   ("required" required (:boolean))))

(json:define-mapping media-type
  :object
  :members
  (("schema" schema schema)
   ("example" example (:any))
   ("examples" examples (:object :value (:or :mappings (example reference))))
   ("encoding" encoding (:object :value 'encoding))))

(json:define-mapping encoding
  :object
  :members
  (("contentType" content-type (:string))
   ("headers" headers (:object :value (:or :mappings (header reference))))
   ("style" style (:string))
   ("explode" explode (:boolean))
   ("allowReserved" allow-reserved (:boolean))))

;; TODO
(json:define-mapping header
  :object
  :members
  ()
  :required
  ())

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
   ("openIdConnectUrl" openid-connect-url (:string)))
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
  (("authorizationUrl" authorization-url (:string))
   ("tokenUrl" token-url (:string))
   ("refreshUrl" refresh-url (:string))
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
   ("url" url (:string)))
  :required
  ("description"))
