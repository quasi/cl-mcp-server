;;; src/packages.lisp
;;; ABOUTME: Package definitions for CL-MCP-Server (uses cl-mcp library)

;;; Slim conditions package — only REPL-specific conditions
;;; All JSON-RPC error conditions come from cl-mcp.conditions
(defpackage #:cl-mcp-server.conditions
  (:use #:cl #:cl-mcp.conditions)
  (:shadowing-import-from #:cl-mcp.conditions #:parse-error)
  (:export
   ;; Re-export from cl-mcp.conditions for backward compat
   #:mcp-error
   #:json-rpc-error
   #:parse-error
   #:invalid-request
   #:method-not-found
   #:invalid-params
   #:internal-error
   #:error-code
   #:error-message
   #:error-data
   ;; REPL-specific
   #:evaluation-timeout
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
   #:with-error-capture
   ;; Phase C: Structured error capture
   #:capture-structured-error
   #:format-structured-error
   #:format-backtrace-detail))

(defpackage #:cl-mcp-server.session
  (:use #:cl #:cl-mcp.conditions)
  (:shadowing-import-from #:cl-mcp.conditions #:parse-error)
  (:export
   #:*session*
   #:session
   #:make-session
   #:session-package
   #:session-loaded-systems
   #:session-definitions
   #:session-last-error
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
   #:result-structured-error
   #:result-success-p
   #:result-definitions
   #:result-timing
   #:result-package
   #:format-result
   #:format-timing-result
   ;; Timeout configuration
   #:*evaluation-timeout*
   #:*max-output-chars*
   #:*include-backtrace-in-evaluate-response*))

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
   ;; A.5: validate-syntax
   #:introspect-validate-syntax
   #:format-validate-result
   ;; B.2: compile-form
   #:introspect-compile-form
   #:format-compile-result
   ;; D.1: class-info
   #:introspect-slot
   #:introspect-class
   #:format-slot-info
   #:format-class-info
   ;; D.2: find-methods
   #:introspect-method
   #:introspect-find-methods
   #:format-method-info
   #:format-find-methods-results
   ;; Helper
   #:resolve-symbol))

(defpackage #:cl-mcp-server.asdf-tools
  (:use #:cl)
  (:export
   ;; E.1: describe-system
   #:introspect-system
   #:format-system-info
   #:collect-components
   ;; E.3: quickload / quicklisp-search
   #:quicklisp-available-p
   #:introspect-quickload
   #:format-quickload-result
   #:introspect-quicklisp-search
   #:format-quicklisp-search-results
   ;; E.4: system-dependencies
   #:introspect-system-dependencies
   #:format-system-dependencies
   ;; E.5: list-local-systems / find-system-file
   #:introspect-local-systems
   #:format-local-systems
   #:introspect-find-system-file
   ;; E.6: load-file
   #:introspect-load-file
   #:format-load-file-result
   ;; Helper
   #:normalize-dependency))

(defpackage #:cl-mcp-server.profiling-tools
  (:use #:cl)
  (:export
   ;; F.1: profile-code (statistical profiling)
   #:introspect-profile-code
   #:format-profile-code-result
   ;; F.2: profile-functions (deterministic profiling)
   #:*profiled-functions*
   #:introspect-profile-functions
   #:format-profile-functions-result
   ;; F.3: memory-report
   #:introspect-memory-report
   #:format-memory-report
   ;; F.4: allocation-profile
   #:introspect-allocation-profile
   #:format-allocation-profile-result))

(defpackage #:cl-mcp-server.telos-tools
  (:use #:cl)
  (:export
   ;; Availability check
   #:telos-available-p
   ;; Name resolution
   #:resolve-feature-name
   #:resolve-feature
   #:qualified-name
   ;; Introspection
   #:introspect-list-features
   #:introspect-feature-intent
   #:introspect-get-intent
   #:introspect-intent-chain
   #:introspect-feature-members
   #:introspect-feature-decisions
   #:introspect-list-decisions
   ;; Formatting
   #:format-list-features
   #:format-feature-intent
   #:format-get-intent
   #:format-intent-chain
   #:format-feature-members
   #:format-feature-decisions
   #:format-list-decisions))

(defpackage #:cl-mcp-server.paren-tools
  (:use #:cl)
  (:export
   #:find-matching-paren
   #:format-match-result))

(defpackage #:cl-mcp-server.tools
  (:use #:cl
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.session
        #:cl-mcp-server.introspection
        #:cl-mcp-server.asdf-tools
        #:cl-mcp-server.profiling-tools
        #:cl-mcp-server.telos-tools
        #:cl-mcp-server.paren-tools)
  (:export
   #:define-builtin-tools
   #:get-usage-guide-content))

(defpackage #:cl-mcp-server
  (:use #:cl
        #:cl-mcp-server.session)
  (:export
   #:start
   #:install-process-guards))
