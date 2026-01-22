#!/usr/bin/env -S sbcl --script
;;; run-server.lisp
;;; ABOUTME: Launcher script for CL-MCP-Server

;;; Try to load Quicklisp from common locations
(flet ((try-load (path)
         (when (probe-file path)
           (load path :verbose nil)
           t)))
  (or (try-load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))
      (try-load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
      (try-load (merge-pathnames ".roswell/lisp/quicklisp/setup.lisp" (user-homedir-pathname)))
      (try-load #p"/usr/local/share/quicklisp/setup.lisp")))

;;; Load ASDF if not already available
(require :asdf)

;;; Add current directory to ASDF search path
(push (make-pathname :directory (pathname-directory *load-truename*))
      asdf:*central-registry*)

;;; Load the MCP server system using Quicklisp if available
(handler-case
    (if (find-package :ql)
        (funcall (intern "QUICKLOAD" :ql) :cl-mcp-server :silent t)
        (asdf:load-system :cl-mcp-server :verbose nil))
  (error (c)
    (format *error-output* "Error loading system: ~a~%" c)
    (sb-ext:exit :code 1)))

;;; Start the server
(cl-mcp-server:start)
