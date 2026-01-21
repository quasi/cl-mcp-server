;;; src/server.lisp
;;; ABOUTME: Main MCP server entry point

(in-package #:cl-mcp-server)

(defparameter *server-info*
  '(("name" . "cl-mcp-server")
    ("version" . "0.1.0"))
  "Server identification for MCP initialize response.")

(defun start ()
  "Start the MCP server. Reads from stdin, writes to stdout."
  (error "Not yet implemented"))
