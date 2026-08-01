;;; src/server.lisp
;;; ABOUTME: CL-MCP-Server entry point — REPL tools over MCP

(in-package #:cl-mcp-server)

(defun start (&key (input *standard-input*) (output *standard-output*))
  "Start the CL REPL MCP server. Reads from stdin, writes to stdout.

INPUT and OUTPUT default to the real stdio. Because keyword defaults are
evaluated before the body runs, they capture the true pipe before
INSTALL-PROCESS-GUARDS redirects the global stream variables — so the
transport keeps talking to the client while everything else goes to stderr."
  (install-process-guards)
  ;; SBCL attributes anything EVAL'd here to the file being loaded — this
  ;; launcher. Record it so cross-reference results can say "defined in this
  ;; session" instead of pointing at a script the user never wrote in.
  (setf cl-mcp-server.introspection:*session-source-pathname* *load-truename*)
  (let ((server (cl-mcp:make-server :name "cl-mcp-server" :version "0.3.0"))
        (session (make-session)))
    (with-session (session)
      (cl-mcp-server.tools:define-builtin-tools server session)
      (cl-mcp:run-server server :input input :output output))))
