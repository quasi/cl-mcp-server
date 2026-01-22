;;; src/transport.lisp
;;; ABOUTME: Stdio transport for MCP communication

(in-package #:cl-mcp-server.transport)

(defun read-message (&optional (stream *standard-input*))
  "Read a single JSON-RPC message from stream.
   Messages are newline-delimited JSON (NDJSON).
   Returns nil on EOF, json-rpc-request on success.
   Signals parse-error or invalid-request on bad input.
   Skips empty lines while waiting for content."
  (loop
    (let ((line (read-line stream nil nil)))
      ;; Actual EOF - return nil
      (unless line
        (return nil))
      ;; Check for content
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (unless (zerop (length trimmed))
          (return (cl-mcp-server.json-rpc:parse-message trimmed)))))))

(defun write-message (response &optional (stream *standard-output*))
  "Write a JSON-RPC response to stream with newline delimiter."
  (write-string (cl-mcp-server.json-rpc:encode-response response) stream)
  (write-char #\Newline stream)
  (force-output stream))

(defmacro with-stdio-transport ((&key (input '*standard-input*)
                                      (output '*standard-output*))
                                &body body)
  "Execute body with stdio transport bindings.
   Useful for testing or redirecting I/O."
  `(let ((*standard-input* ,input)
         (*standard-output* ,output))
     ,@body))
