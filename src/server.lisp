;;; src/server.lisp
;;; ABOUTME: Main MCP server entry point and request handlers

(in-package #:cl-mcp-server)

;;; ==========================================================================
;;; Server Configuration
;;; ==========================================================================

(defparameter *server-info*
  '(("name" . "cl-mcp-server")
    ("version" . "0.1.0"))
  "Server identification for MCP initialize response.")

(defparameter *protocol-version* "2025-06-18"
  "MCP protocol version supported by this server.")

(defvar *server-session* nil
  "The current server session.")

;;; ==========================================================================
;;; MCP Request Handlers
;;; ==========================================================================

(defun handle-initialize (id)
  "Handle the initialize request.
Returns server info and capabilities."
  (let ((empty-obj (make-hash-table :test #'equal)))
    (make-success-response
     :id id
     :result `(("protocolVersion" . ,*protocol-version*)
               ("serverInfo" . ,*server-info*)
               ("capabilities" . (("tools" . ,empty-obj)))))))

(defun handle-tools-list (id)
  "Handle the tools/list request.
Returns a list of available tools."
  (make-success-response
   :id id
   :result `(("tools" . ,(cl-mcp-server.tools:tools-for-mcp)))))

(defun handle-tools-call (id params)
  "Handle the tools/call request.
Calls the specified tool with the given arguments."
  (let ((name (cdr (assoc "name" params :test #'string=)))
        (arguments (cdr (assoc "arguments" params :test #'string=))))
    (handler-case
        (let ((result (cl-mcp-server.tools:call-tool
                       name
                       arguments
                       *session*)))
          (make-success-response
           :id id
           :result `(("content" . ((("type" . "text")
                                    ("text" . ,result)))))))
      (method-not-found (c)
        (make-error-response
         :id id
         :code (error-code c)
         :message (error-message c)))
      (invalid-params (c)
        (make-error-response
         :id id
         :code (error-code c)
         :message (error-message c))))))

;;; ==========================================================================
;;; Request Dispatcher
;;; ==========================================================================

(defun handle-request (request)
  "Handle a JSON-RPC request and return a response.
Returns nil for notifications (no id).
Dispatches to the appropriate handler based on method."
  ;; Notifications don't get responses
  (when (notification-p request)
    (return-from handle-request nil))
  (let ((id (request-id request))
        (method (request-method request))
        (params (request-params request)))
    (handler-case
        (cond
          ((string= method "initialize")
           (handle-initialize id))
          ((string= method "tools/list")
           (handle-tools-list id))
          ((string= method "tools/call")
           (handle-tools-call id params))
          (t
           (error 'method-not-found
                  :message (format nil "Method not found: ~a" method))))
      (method-not-found (c)
        (make-error-response
         :id id
         :code (error-code c)
         :message (error-message c)))
      (invalid-params (c)
        (make-error-response
         :id id
         :code (error-code c)
         :message (error-message c)))
      (error (c)
        (make-error-response
         :id id
         :code -32603
         :message (format nil "Internal error: ~a" c))))))

;;; ==========================================================================
;;; Server Main Loop
;;; ==========================================================================

(defun run-server (&key (input *standard-input*) (output *standard-output*))
  "Run the MCP server main loop.
Reads JSON-RPC requests from INPUT and writes responses to OUTPUT.
Maintains a session for the duration of the server run.
Returns when EOF is reached on INPUT."
  (format *error-output* "MCP Server starting main loop~%")
  (force-output *error-output*)
  (let ((*server-session* (make-session)))
    (with-session (*server-session*)
      (loop
        (handler-case
            (progn
              (format *error-output* "Waiting for message...~%")
              (force-output *error-output*)
              (let ((request (read-message input)))
                ;; EOF - exit the loop
                (unless request
                  (format *error-output* "EOF received, exiting~%")
                  (force-output *error-output*)
                  (return))
                (format *error-output* "Received request: ~a~%" (request-method request))
                (force-output *error-output*)
                ;; Process request and send response
                (let ((response (handle-request request)))
                  (when response
                    (format *error-output* "Sending response~%")
                    (force-output *error-output*)
                    (write-message response output)))))
          ;; Handle parse/protocol errors
          (json-rpc-error (c)
            (format *error-output* "JSON-RPC error: ~a~%" c)
            (force-output *error-output*)
            (write-message
             (make-error-response
              :id nil
              :code (error-code c)
              :message (error-message c))
             output))
          ;; Handle unexpected errors
          (error (c)
            (format *error-output* "Unexpected error: ~a~%" c)
            (force-output *error-output*)
            (write-message
             (make-error-response
              :id nil
              :code -32603
              :message (format nil "Internal error: ~a" c))
             output)))))))

(defun start ()
  "Start the MCP server. Reads from stdin, writes to stdout."
  (run-server))
