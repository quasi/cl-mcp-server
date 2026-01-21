;;; src/packages.lisp
;;; ABOUTME: Package definitions for CL-MCP-Server

(defpackage #:cl-mcp-server.conditions
  (:use #:cl)
  (:shadow #:parse-error)
  (:export
   ;; Condition types
   #:mcp-error
   #:json-rpc-error
   #:parse-error
   #:invalid-request
   #:method-not-found
   #:invalid-params
   #:internal-error
   ;; Condition accessors
   #:error-code
   #:error-message
   #:error-data))

(defpackage #:cl-mcp-server.json-rpc
  (:use #:cl #:cl-mcp-server.conditions)
  (:shadowing-import-from #:cl-mcp-server.conditions #:parse-error)
  (:export
   ;; Message types
   #:json-rpc-request
   #:json-rpc-response
   #:json-rpc-error-response
   #:json-rpc-notification
   ;; Accessors
   #:request-id
   #:request-method
   #:request-params
   #:response-id
   #:response-result
   #:response-error
   ;; Constructors
   #:make-request
   #:make-notification
   #:notification-p
   #:make-success-response
   #:make-error-response
   ;; Functions (future)
   #:parse-message
   #:encode-response
   #:encode-error))

(defpackage #:cl-mcp-server.transport
  (:use #:cl #:cl-mcp-server.json-rpc)
  (:export
   #:read-message
   #:write-message
   #:with-stdio-transport))

(defpackage #:cl-mcp-server.session
  (:use #:cl)
  (:export
   #:*session*
   #:session
   #:make-session
   #:session-package
   #:session-definitions
   #:reset-session
   #:list-definitions
   #:with-session))

(defpackage #:cl-mcp-server.evaluator
  (:use #:cl #:cl-mcp-server.session)
  (:export
   #:evaluate-code
   #:evaluation-result
   #:result-values
   #:result-stdout
   #:result-stderr
   #:result-warnings
   #:result-error
   #:result-success-p))

(defpackage #:cl-mcp-server.tools
  (:use #:cl
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.session)
  (:export
   #:*tools*
   #:tool-definition
   #:get-tool
   #:call-tool
   #:list-tools))

(defpackage #:cl-mcp-server
  (:use #:cl
        #:cl-mcp-server.conditions
        #:cl-mcp-server.json-rpc
        #:cl-mcp-server.transport
        #:cl-mcp-server.session
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.tools)
  (:shadowing-import-from #:cl-mcp-server.conditions #:parse-error)
  (:export
   #:start
   #:*server-info*))
