#!/usr/bin/env -S sbcl --script
;;; run-server.lisp
;;; ABOUTME: Launcher script for CL-MCP-Server

;;; Redirect all output to stderr during setup
;;; MCP requires stdout to only contain JSON-RPC messages
(let ((*standard-output* *error-output*)
      (*trace-output* *error-output*))

  ;;; Try to load Quicklisp from common locations
  (flet ((try-load (path)
           (when (probe-file path)
             (load path :verbose nil :print nil)
             t)))
    (or (try-load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))
        (try-load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
        (try-load (merge-pathnames ".roswell/lisp/quicklisp/setup.lisp" (user-homedir-pathname)))
        (try-load #p"/usr/local/share/quicklisp/setup.lisp")))

  ;;; Load ASDF if not already available
  (require "asdf")

  ;;; Add current directory to ASDF search path
  (push (make-pathname :directory (pathname-directory *load-truename*))
        (symbol-value (find-symbol "*CENTRAL-REGISTRY*" "ASDF")))

  ;;; Load the MCP server system using Quicklisp if available
  (handler-case
      (if (find-package "QL")
          (funcall (find-symbol "QUICKLOAD" "QL") "cl-mcp-server" :silent t)
          (funcall (find-symbol "LOAD-SYSTEM" "ASDF") "cl-mcp-server" :verbose nil))
    (error (c)
      (format *error-output* "Error loading system: ~a~%" c)
      (funcall (find-symbol "EXIT" "SB-EXT") :code 1))))

;;; Start the server (output goes to real stdout now)
(funcall (find-symbol "START" "CL-MCP-SERVER"))
