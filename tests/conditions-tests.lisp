;;; tests/conditions-tests.lisp
;;; ABOUTME: Tests for MCP condition types

(in-package #:cl-mcp-server-tests)

(def-suite conditions-tests
  :description "Condition type tests"
  :in cl-mcp-server-tests)

(in-suite conditions-tests)

(test json-rpc-error-codes
  "JSON-RPC error conditions have correct codes"
  (is (= -32700 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:parse-error))))
  (is (= -32600 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:invalid-request))))
  (is (= -32601 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:method-not-found))))
  (is (= -32602 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:invalid-params))))
  (is (= -32603 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:internal-error)))))

(test json-rpc-error-messages
  "JSON-RPC error conditions have messages"
  (let ((err (make-condition 'cl-mcp-server.conditions:method-not-found
                             :message "Method foo not found")))
    (is (string= "Method foo not found"
                 (cl-mcp-server.conditions:error-message err)))))
