;;; cl-mcp-server.asd
;;; ABOUTME: ASDF system definition for CL-MCP-Server

(asdf:defsystem #:cl-mcp-server
  :description "Model Context Protocol server for Common Lisp evaluation"
  :author "Baba <quasi@quasilabs.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:yason           ; JSON parsing
               #:alexandria      ; Utilities
               #:bordeaux-threads ; Threading (future)
               #:trivial-backtrace) ; Portable backtraces
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "conditions")
                 (:file "error-format")
                 (:file "json-rpc")
                 (:file "transport")
                 (:file "session")
                 (:file "evaluator")
                 (:file "introspection")
                 (:file "asdf-tools")
                 (:file "profiling-tools")
                 (:file "tools")
                 (:file "server"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-mcp-server/tests))))

(asdf:defsystem #:cl-mcp-server/tests
  :description "Tests for CL-MCP-Server"
  :depends-on (#:cl-mcp-server
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "conditions-tests")
                 (:file "error-format-tests")
                 (:file "json-rpc-tests")
                 (:file "transport-tests")
                 (:file "session-tests")
                 (:file "evaluator-tests")
                 (:file "tools-tests")
                 (:file "introspection-tests")
                 (:file "asdf-tools-tests")
                 (:file "profiling-tools-tests")
                 (:file "integration-tests")
                 (:file "encoding-tests"))))
  :perform (asdf:test-op (o c)
             (symbol-call :fiveam :run! :cl-mcp-server-tests)))
