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
               cl-mcp-server.evaluator:*max-output-chars*))))

  ;; ========================================================================
  ;; Phase A: Introspection Tools
  ;; ========================================================================

  ;; describe-symbol: Get comprehensive symbol information
  (register-tool
   "describe-symbol"
   "Get comprehensive information about a Lisp symbol including its type, value, documentation, arglist, and source location. Uses SBCL introspection for detailed information."
   '(("type" . "object")
     ("required" . ("name"))
     ("properties" . (("name" . (("type" . "string")
                                 ("description" . "Symbol name to describe")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package name (defaults to CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((name (cdr (assoc "name" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (multiple-value-bind (sym status)
               (find-symbol (string-upcase name) pkg)
             (if sym
                 (format-symbol-info (introspect-symbol sym))
                 (format nil "Symbol ~A not found in package ~A (status: ~A)"
                         name (package-name pkg) status)))))))

  ;; apropos-search: Search for symbols matching a pattern
  (register-tool
   "apropos-search"
   "Search for symbols matching a pattern. Returns symbol names, types, and packages. Useful for discovering available functions, variables, and classes."
   '(("type" . "object")
     ("required" . ("pattern"))
     ("properties" . (("pattern" . (("type" . "string")
                                    ("description" . "Search pattern (case-insensitive substring)")))
                      ("package" . (("type" . "string")
                                    ("description" . "Limit search to this package (optional)")))
                      ("type" . (("type" . "string")
                                 ("description" . "Filter by type: function, macro, variable, class, generic-function")
                                 ("enum" . ("function" "macro" "variable" "class" "generic-function")))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((pattern (cdr (assoc "pattern" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (type-str (cdr (assoc "type" args :test #'string=)))
            (type-kw (when type-str
                       (intern (string-upcase type-str) :keyword))))
       (if (and pkg-name (not (find-package (string-upcase pkg-name))))
           (format nil "Package ~A not found" pkg-name)
           (let ((results (introspect-apropos pattern
                                              :package pkg-name
                                              :type type-kw)))
             (format-apropos-results results pattern))))))

  ;; who-calls: Find all functions that call a specified function
  (register-tool
   "who-calls"
   "Find all functions that call the specified function. Uses SBCL's cross-reference database to track callers."
   '(("type" . "object")
     ("required" . ("name"))
     ("properties" . (("name" . (("type" . "string")
                                 ("description" . "Function name to find callers of")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package name (defaults to CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((name (cdr (assoc "name" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (let ((sym (find-symbol (string-upcase name) pkg)))
             (if sym
                 (let ((results (introspect-who-calls sym)))
                   (format-who-calls-results results sym))
                 (format nil "Symbol ~A not found in package ~A" name (package-name pkg))))))))

  ;; who-references: Find all code that references a variable
  (register-tool
   "who-references"
   "Find all code that references (reads) the specified variable or constant. Uses SBCL's cross-reference database."
   '(("type" . "object")
     ("required" . ("name"))
     ("properties" . (("name" . (("type" . "string")
                                 ("description" . "Variable name to find references to")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package name (defaults to CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((name (cdr (assoc "name" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (let ((sym (find-symbol (string-upcase name) pkg)))
             (if sym
                 (let ((results (introspect-who-references sym)))
                   (format-who-references-results results sym))
                 (format nil "Symbol ~A not found in package ~A" name (package-name pkg))))))))

  ;; macroexpand-form: Expand macros in a form
  (register-tool
   "macroexpand-form"
   "Expand macros in a Lisp form. Useful for understanding macro transformations and debugging macro usage."
   '(("type" . "object")
     ("required" . ("form"))
     ("properties" . (("form" . (("type" . "string")
                                 ("description" . "Lisp form to expand (as a string)")))
                      ("full" . (("type" . "boolean")
                                 ("description" . "If true, fully expand all macros recursively. Default: false (one step only)")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package context for reading the form (defaults to CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((form-str (cdr (assoc "form" args :test #'string=)))
            (full (cdr (assoc "full" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (handler-case
               (let ((result (introspect-macroexpand form-str :full full :package pkg)))
                 (format-macroexpand-result result))
             (error (e)
               (format nil "Error expanding form: ~A" e))))))))

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
