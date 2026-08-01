;;; cl-mcp-server.asd
;;; ABOUTME: ASDF system definition for CL-MCP-Server

(asdf:defsystem #:cl-mcp-server
  :description "Model Context Protocol server for Common Lisp evaluation"
  :author "Abhijit Rao <quasi@quasilabs.com>"
  :license "MIT"
  :version "0.3.0"
  :serial t
  :depends-on (#:cl-mcp            ; MCP protocol framework
               #:alexandria        ; Utilities
               #:bordeaux-threads  ; Threading (future)
               #:trivial-backtrace) ; Portable backtraces
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "conditions")
                 (:file "error-format")
                 (:file "session")
                 (:file "evaluator")
                 (:file "introspection")
                 (:file "asdf-tools")
                 (:file "profiling-tools")
                 (:file "telos-tools")
                 (:file "paren-tools")
                 (:file "tools")
                 (:file "stdio-guard")
                 (:file "server"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-mcp-server/tests))))

(asdf:defsystem #:cl-mcp-server/tests
  :description "Tests for CL-MCP-Server"
  :depends-on (#:cl-mcp-server
               #:fiveam
               ;; Test-only. cl-mcp-server itself degrades gracefully when
               ;; telos is absent, but the name-resolution tests are only
               ;; meaningful against telos's real registry shape.
               #:telos)
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "telos-fixture")
                 (:file "error-format-tests")
                 (:file "session-tests")
                 (:file "evaluator-tests")
                 (:file "tools-tests")
                 (:file "introspection-tests")
                 (:file "asdf-tools-tests")
                 (:file "profiling-tools-tests")
                 (:file "paren-tools-tests")
                 (:file "telos-tools-tests")
                 (:file "integration-tests")
                 (:file "transport-isolation-tests"))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :fiveam :run! :cl-mcp-server-tests)))
