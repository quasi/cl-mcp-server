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
   #:evaluation-timeout
   ;; Condition accessors
   #:error-code
   #:error-message
   #:error-data
   #:timeout-seconds
   #:timeout-backtrace))

(defpackage #:cl-mcp-server.error-format
  (:use #:cl)
  (:export
   #:format-condition
   #:format-error
   #:format-warning
   #:format-backtrace
   #:*max-backtrace-depth*
   #:*print-backtrace-p*
   #:with-error-capture))

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
  (:use #:cl #:cl-mcp-server.conditions)
  (:shadowing-import-from #:cl-mcp-server.conditions #:parse-error)
  (:export
   #:*session*
   #:session
   #:make-session
   #:session-package
   #:session-loaded-systems
   #:session-definitions
   #:reset-session
   #:list-definitions
   #:format-definitions
   #:switch-package
   #:with-session))

(defpackage #:cl-mcp-server.evaluator
  (:use #:cl
        #:cl-mcp-server.session
        #:cl-mcp-server.error-format
        #:cl-mcp-server.conditions)
  (:shadowing-import-from #:cl-mcp-server.conditions #:parse-error)
  (:export
   #:evaluate-code
   #:evaluation-result
   #:make-evaluation-result
   #:result-values
   #:result-stdout
   #:result-stderr
   #:result-warnings
   #:result-error
   #:result-success-p
   #:result-definitions
   #:format-result
   ;; Timeout configuration
   #:*evaluation-timeout*
   #:*max-output-chars*))

(defpackage #:cl-mcp-server.introspection
  (:use #:cl)
  (:export
   ;; Symbol type classification
   #:symbol-type-info
   ;; A.1: describe-symbol
   #:introspect-symbol
   #:format-symbol-info
   ;; A.2: apropos-search
   #:introspect-apropos
   #:format-apropos-results
   ;; A.3: who-calls
   #:introspect-who-calls
   #:format-who-calls-results
   ;; who-references
   #:introspect-who-references
   #:format-who-references-results
   ;; A.4: macroexpand-form
   #:introspect-macroexpand
   #:format-macroexpand-result
   ;; Helper
   #:resolve-symbol))

(defpackage #:cl-mcp-server.tools
  (:use #:cl
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.session
        #:cl-mcp-server.introspection)
  (:export
   #:*tools*
   #:tool-definition
   #:tool-name
   #:tool-description
   #:tool-input-schema
   #:tool-handler
   #:register-tool
   #:get-tool
   #:list-tools
   #:tools-for-mcp
   #:call-tool
   #:validate-tool-args))

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
   #:run-server
   #:*server-info*
   #:*server-session*))
