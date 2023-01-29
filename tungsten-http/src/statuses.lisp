(in-package :http)

(defparameter *response-status-reasons*
  (core:alist-to-hash-table
   '(;; RFC 7231 (HTTP/1.1 Semantics and Content)
     (100 . "Continue")
     (101 . "Switching Protocols")
     (200 . "OK")
     (201 . "Created")
     (202 . "Accepted")
     (203 . "Non-Authoritative Information")
     (204 . "No Content")
     (205 . "Reset Content")
     (300 . "Multiple Choices")
     (301 . "Moved Permanently")
     (302 . "Found")
     (303 . "See Other")
     (305 . "Use Proxy")
     (307 . "Temporary Redirect")
     (400 . "Bad Request")
     (402 . "Payment Required")
     (403 . "Forbidden")
     (404 . "Not Found")
     (405 . "Method Not Allowed")
     (406 . "Not Acceptable")
     (408 . "Request Timeout")
     (409 . "Conflict")
     (410 . "Gone")
     (411 . "Length Required")
     (413 . "Payload Too Large")
     (414 . "URI Too Long")
     (415 . "Unsupported Media Type")
     (417 . "Expectation Failed")
     (426 . "Upgrade Required")
     (500 . "Internal Server Error")
     (501 . "Not Implemented")
     (502 . "Bad Gateway")
     (503 . "Service Unavailable")
     (504 . "Gateway Timeout")
     (505 . "HTTP Version Not Supported")

     ;; RFC 2295 (Transparent Content Negotiation in HTTP)
     (506 . "Variant Also Negotiates")

     ;; RFC 2324 (Hyper Text Coffee Pot Control Protocol (HTCPCP/1.0))
     (418 . "I'm a teapot")

     ;; RFC 2518 (HTTP Extensions for Distributed Authoring -- WEBDAV)
     (102 . "Processing")

     ;; RFC 2774 (An HTTP Extension Framework)
     (510 . "Not Extended")

     ;; RFC 3229
     (226 . "IM Used")

     ;; RFC 4918 (WebDAV)
     (207 . "Multi-Status")
     (422 . "Unprocessable Entity")
     (423 . "Locked")
     (424 . "Failed Dependency")
     (507 . "Insufficient Storage")

     ;; RFC 5842 (Binding Extensions to WebDAV)
     (208 . "Already Reported")
     (508 . "Loop Detected")

     ;; RFC 6585 (Additional HTTP Status Codes)
     (428 . "Precondition Required")
     (429 . "Too Many Requests")
     (431 . "Request Header Fields Too Large")
     (511 . "Network Authentication Required")

     ;; RFC 7232 (HTTP/1.1 Conditional Requests)
     (304 . "Not Modified")
     (412 . "Precondition Failed")

     ;; RFC 7233 (HTTP/1.1 Range Requests)
     (206 . "Partial Content")
     (416 . "Range Not Satisfiable")

     ;; RFC 7235 (HTTP/1.1 Authentication)
     (401 . "Unauthorized")
     (407 . "Proxy Authentication Required")

     ;; RFC 7538 (HTTP Status Code 308)
     (308 . "Permanent Redirect")

     ;; RFC 7725 (An HTTP Status Code to Report Legal Obstacles)
     (451 . "Unavailable For Legal Reasons")

     ;; RFC 8297 (An HTTP Status Code for Indicating Hints)
     (103 . "Early Hints")

     ;; RFC 8470 (HTTP Early Data)
     (425 . "Too Early"))))

(defun response-status-reason (status)
  (declare (type response-status status))
  (gethash status *response-status-reasons* "Unknown"))
