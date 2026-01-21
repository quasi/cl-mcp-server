;;; tests/packages.lisp
;;; ABOUTME: Test package definitions

(defpackage #:cl-mcp-server-tests
  (:use #:cl #:fiveam)
  (:export #:run-all-tests))

(in-package #:cl-mcp-server-tests)

(def-suite cl-mcp-server-tests
  :description "All tests for CL-MCP-Server")

(defun run-all-tests ()
  (run! 'cl-mcp-server-tests))
