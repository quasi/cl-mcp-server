;;; src/tools.lisp
;;; ABOUTME: MCP tool definitions and dispatch

(in-package #:cl-mcp-server.tools)

;;; ==========================================================================
;;; Tool Definition Structure
;;; ==========================================================================

(defstruct (tool-definition (:conc-name tool-))
  "Definition of an MCP tool"
  (name "" :type string)
  (description "" :type string)
  (input-schema nil :type list)
  (handler nil :type (or function null)))

;;; ==========================================================================
;;; Tool Registry
;;; ==========================================================================

(defvar *tools* (make-hash-table :test #'equal)
  "Hash table mapping tool names to tool definitions")

(defun register-tool (name description input-schema handler)
  "Register a tool with the given NAME, DESCRIPTION, INPUT-SCHEMA, and HANDLER.
HANDLER is a function that takes (args session) and returns a result string."
  (setf (gethash name *tools*)
        (make-tool-definition
         :name name
         :description description
         :input-schema input-schema
         :handler handler)))

(defun get-tool (name)
  "Get a tool definition by NAME. Returns nil if not found."
  (gethash name *tools*))

(defun list-tools ()
  "Return a list of all registered tool definitions."
  (loop for tool being the hash-values of *tools*
        collect tool))

(defun tools-for-mcp ()
  "Format all tools for MCP tools/list response.
Returns a list of alists with name, description, and inputSchema."
  (loop for tool being the hash-values of *tools*
        collect `(("name" . ,(tool-name tool))
                  ("description" . ,(tool-description tool))
                  ("inputSchema" . ,(tool-input-schema tool)))))

;;; ==========================================================================
;;; Tool Definitions
;;; ==========================================================================

(defun define-builtin-tools ()
  "Define the built-in MCP tools."

  ;; evaluate-lisp: Evaluate Common Lisp code
  (register-tool
   "evaluate-lisp"
   "Evaluate Common Lisp code in the current session. The code is evaluated in sequence and the result of the last form is returned. Output to *standard-output* is captured."
   '(("type" . "object")
     ("required" . ("code"))
     ("properties" . (("code" . (("type" . "string")
                                 ("description" . "Common Lisp code to evaluate"))))))
   (lambda (args session)
     (let* ((code (cdr (assoc "code" args :test #'string=)))
            (result (evaluate-code code)))
       ;; Track definitions in the session
       (when (and session (result-definitions result))
         (setf (session-definitions session)
               (append (result-definitions result)
                       (session-definitions session))))
       (format-result result))))

  ;; list-definitions: List definitions in the current session
  (register-tool
   "list-definitions"
   "List all definitions (functions, variables, macros) in the current session."
   `(("type" . "object")
     ("properties" . (("type" . (("type" . "string")
                                 ("description" . "Optional filter: function, variable, or macro")
                                 ("enum" . ("function" "variable" "macro")))))))
   (lambda (args session)
     (let ((type-filter (cdr (assoc "type" args :test #'string=))))
       (format-definitions session
                           :type (when type-filter
                                   (intern (string-upcase type-filter) :keyword))))))

  ;; reset-session: Reset the session to a fresh state
  (register-tool
   "reset-session"
   "Reset the session to a fresh state, clearing all definitions and loaded systems."
   `(("type" . "object")
     ("properties" . ,(make-hash-table :test #'equal)))
   (lambda (args session)
     (declare (ignore args))
     (cl-mcp-server.session:reset-session session)
     "Session reset successfully."))

  ;; load-system: Load an ASDF system
  (register-tool
   "load-system"
   "Load an ASDF system by name. The system must be findable by ASDF."
   '(("type" . "object")
     ("required" . ("system-name"))
     ("properties" . (("system-name" . (("type" . "string")
                                        ("description" . "Name of the ASDF system to load"))))))
   (lambda (args session)
     (let ((system-name (cdr (assoc "system-name" args :test #'string=))))
       (handler-case
           (progn
             (asdf:load-system system-name)
             (push system-name (session-loaded-systems session))
             (format nil "System ~a loaded successfully." system-name))
         (error (c)
           (format nil "Error loading system ~a: ~a" system-name c))))))

  ;; configure-limits: Configure evaluation safety limits
  (register-tool
   "configure-limits"
   "Configure evaluation safety limits. Returns current configuration after any changes."
   '(("type" . "object")
     ("properties" . (("timeout" . (("type" . "integer")
                                    ("description" . "Evaluation timeout in seconds (default: 30). Set to 0 to disable (not recommended).")))
                      ("max-output" . (("type" . "integer")
                                       ("description" . "Maximum output characters to capture (default: 100000)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let ((timeout (cdr (assoc "timeout" args :test #'string=)))
           (max-output (cdr (assoc "max-output" args :test #'string=))))
       ;; Apply changes if provided
       (when timeout
         (setf cl-mcp-server.evaluator:*evaluation-timeout*
               (if (zerop timeout) nil timeout)))
       (when max-output
         (setf cl-mcp-server.evaluator:*max-output-chars* max-output))
       ;; Return current configuration
       (format nil "Current limits:~%  timeout: ~A seconds~A~%  max-output: ~A characters"
               (or cl-mcp-server.evaluator:*evaluation-timeout* "disabled")
               (if cl-mcp-server.evaluator:*evaluation-timeout* "" " (WARNING: no timeout)")
               cl-mcp-server.evaluator:*max-output-chars*)))))

;;; ==========================================================================
;;; Tool Argument Validation
;;; ==========================================================================

(defun validate-tool-args (args schema)
  "Validate ARGS against the tool's input SCHEMA.
ARGS is an alist of argument names to values.
SCHEMA is a JSON Schema object (as an alist).
Signals INVALID-PARAMS if required arguments are missing.
Returns T if validation passes."
  (let ((required (cdr (assoc "required" schema :test #'string=))))
    (dolist (req-name required)
      (unless (assoc req-name args :test #'string=)
        (error 'cl-mcp-server.conditions:invalid-params
               :message (format nil "Missing required argument: ~a" req-name)))))
  t)

;;; ==========================================================================
;;; Tool Calling
;;; ==========================================================================

(defun call-tool (name args session)
  "Call tool NAME with ARGS in the context of SESSION.
Signals METHOD-NOT-FOUND if the tool doesn't exist.
Signals INVALID-PARAMS if required arguments are missing.
Returns the result string from the tool handler."
  (let ((tool (get-tool name)))
    (unless tool
      (error 'cl-mcp-server.conditions:method-not-found
             :message (format nil "Tool not found: ~a" name)))
    (validate-tool-args args (tool-input-schema tool))
    (funcall (tool-handler tool) args session)))

;; Initialize built-in tools on load
(define-builtin-tools)
