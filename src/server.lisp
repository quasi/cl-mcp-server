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

(defparameter *protocol-version* "2024-11-05"
  "MCP protocol version supported by this server.")

(defvar *server-session* nil
  "The current server session.")

;;; ==========================================================================
;;; MCP Request Handlers
;;; ==========================================================================

(defun handle-initialize (id)
  "Handle the initialize request.
Returns server info and capabilities."
  (make-success-response
   :id id
   :result `(("protocolVersion" . ,*protocol-version*)
             ("serverInfo" . ,*server-info*)
             ("capabilities" . (("tools" . ()))))))

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
