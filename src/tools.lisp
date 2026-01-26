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

  ;; ========================================================================
  ;; Usage Guide - Call first to understand best practices
  ;; ========================================================================

  (register-tool
   "get-usage-guide"
   "Get the recommended workflow guide for using this Lisp MCP server effectively. RECOMMENDED: Call this when starting a new session to learn best practices for incremental development, syntax validation, and effective tool usage."
   `(("type" . "object")
     ("properties" . ,(make-hash-table :test #'equal)))
   (lambda (args session)
     (declare (ignore args session))
     (get-usage-guide-content)))

  ;; evaluate-lisp: Evaluate Common Lisp code with enhanced feedback
  (register-tool
   "evaluate-lisp"
   "Evaluate Common Lisp code in the current session. The code is evaluated in sequence and the result of the last form is returned. Output to *standard-output* is captured."
   '(("type" . "object")
     ("required" . ("code"))
     ("properties" . (("code" . (("type" . "string")
                                 ("description" . "Common Lisp code to evaluate")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package context for evaluation (default: CL-USER)")))
                      ("capture-time" . (("type" . "boolean")
                                         ("description" . "Include timing information in result"))))))
   (lambda (args session)
     (let* ((code (cdr (assoc "code" args :test #'string=)))
            (pkg (cdr (assoc "package" args :test #'string=)))
            (capture-time (cdr (assoc "capture-time" args :test #'string=)))
            (result (evaluate-code code :package pkg :capture-time capture-time)))
       ;; Track definitions in the session
       (when (and session (result-definitions result))
         (setf (session-definitions session)
               (append (result-definitions result)
                       (session-definitions session))))
       ;; Phase C: Store structured error in session for later inspection
       (when (and session (result-structured-error result))
         (setf (session-last-error session) (result-structured-error result)))
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
               (format nil "Error expanding form: ~A" e)))))))

  ;; validate-syntax: Validate code syntax without evaluation
  (register-tool
   "validate-syntax"
   "Check if Common Lisp code is syntactically valid without evaluating it. Detects unbalanced parentheses, reader errors, and other syntax issues. Use this to verify code before saving or executing."
   '(("type" . "object")
     ("required" . ("code"))
     ("properties" . (("code" . (("type" . "string")
                                 ("description" . "Common Lisp code to validate"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((code (cdr (assoc "code" args :test #'string=)))
            (result (introspect-validate-syntax code)))
       (format-validate-result result))))

  ;; ========================================================================
  ;; Phase B: Enhanced Evaluation Tools
  ;; ========================================================================

  ;; compile-form: Compile code without evaluating it
  (register-tool
   "compile-form"
   "Compile Common Lisp code without executing it. Catches compilation warnings, type errors, and other issues that only appear at compile time. Useful for checking code correctness before evaluation."
   '(("type" . "object")
     ("required" . ("code"))
     ("properties" . (("code" . (("type" . "string")
                                 ("description" . "Common Lisp code to compile")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package context for compilation (default: CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((code (cdr (assoc "code" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (result (introspect-compile-form code :package (or pkg-name "CL-USER"))))
       (format-compile-result result))))

  ;; time-execution: Execute code with detailed timing
  (register-tool
   "time-execution"
   "Execute code with detailed timing and memory allocation information. Returns real time, run time, GC time, and bytes allocated. Useful for profiling and performance analysis."
   '(("type" . "object")
     ("required" . ("code"))
     ("properties" . (("code" . (("type" . "string")
                                 ("description" . "Common Lisp code to execute and time")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package context for execution (default: CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((code (cdr (assoc "code" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (result (evaluate-code code :package pkg-name :capture-time t)))
       (format-timing-result result))))

  ;; ========================================================================
  ;; Phase C: Error Intelligence Tools
  ;; ========================================================================

  ;; describe-last-error: Get detailed info about the most recent error
  (register-tool
   "describe-last-error"
   "Get detailed information about the most recent error. Returns the error type, message, available restarts, and backtrace from the last failed evaluation. Useful for diagnosing why code failed."
   `(("type" . "object")
     ("properties" . ,(make-hash-table :test #'equal)))
   (lambda (args session)
     (declare (ignore args))
     (if (and session (session-last-error session))
         (cl-mcp-server.error-format:format-structured-error
          (session-last-error session))
         "No error recorded in this session. Run some code that causes an error first.")))

  ;; get-backtrace: Get stack trace from the last error
  (register-tool
   "get-backtrace"
   "Get the stack trace from the most recent error. Shows the call stack at the point where the error occurred, with frame numbers and function calls. Use max-frames to limit output."
   '(("type" . "object")
     ("properties" . (("max-frames" . (("type" . "integer")
                                       ("description" . "Maximum number of frames to return (default: 20)"))))))
   (lambda (args session)
     (let ((max-frames (or (cdr (assoc "max-frames" args :test #'string=)) 20)))
       (if (and session (session-last-error session))
           (cl-mcp-server.error-format:format-backtrace-detail
            (session-last-error session) :max-frames max-frames)
           "No error recorded in this session. Run some code that causes an error first."))))

  ;; ========================================================================
  ;; Phase D: CLOS Intelligence Tools
  ;; ========================================================================

  ;; class-info: Get complete class information
  (register-tool
   "class-info"
   "Get complete information about a CLOS class including its slots, superclasses, subclasses, and metaclass. Works with any class in the running image."
   '(("type" . "object")
     ("required" . ("class"))
     ("properties" . (("class" . (("type" . "string")
                                  ("description" . "Class name to inspect")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package name (defaults to CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((class-name (cdr (assoc "class" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (let ((sym (find-symbol (string-upcase class-name) pkg)))
             (if (and sym (find-class sym nil))
                 (handler-case
                     (format-class-info (introspect-class sym))
                   (error (e)
                     (format nil "Error inspecting class: ~A" e)))
                 (format nil "Class ~A not found in package ~A"
                         class-name (package-name pkg))))))))

  ;; find-methods: Find all methods specialized on a class
  (register-tool
   "find-methods"
   "Find all methods that are specialized on a given class. Shows the generic function name, qualifiers, and specializers for each method. Useful for understanding what operations are available on a class."
   '(("type" . "object")
     ("required" . ("class"))
     ("properties" . (("class" . (("type" . "string")
                                  ("description" . "Class name to find methods for")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package name (defaults to CL-USER)")))
                      ("include-inherited" . (("type" . "boolean")
                                              ("description" . "Include methods from superclasses (default: false)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((class-name (cdr (assoc "class" args :test #'string=)))
            (pkg-name (cdr (assoc "package" args :test #'string=)))
            (include-inherited (cdr (assoc "include-inherited" args :test #'string=)))
            (pkg (if pkg-name
                     (find-package (string-upcase pkg-name))
                     (find-package "CL-USER"))))
       (if (not pkg)
           (format nil "Package ~A not found" pkg-name)
           (let ((sym (find-symbol (string-upcase class-name) pkg)))
             (if (and sym (find-class sym nil))
                 (handler-case
                     (let ((results (introspect-find-methods sym
                                                             :include-inherited include-inherited)))
                       (format-find-methods-results results sym))
                   (error (e)
                     (format nil "Error finding methods: ~A" e)))
                 (format nil "Class ~A not found in package ~A"
                         class-name (package-name pkg))))))))

  ;; ========================================================================
  ;; Phase E: ASDF & Quicklisp Integration Tools
  ;; ========================================================================

  ;; describe-system: Get ASDF system information
  (register-tool
   "describe-system"
   "Get comprehensive information about an ASDF system including its version, description, author, license, components, and dependencies. Use this to understand a system's structure before loading."
   '(("type" . "object")
     ("required" . ("system"))
     ("properties" . (("system" . (("type" . "string")
                                   ("description" . "Name of the ASDF system"))))))
   (lambda (args session)
     (declare (ignore session))
     (let ((system-name (cdr (assoc "system" args :test #'string=))))
       (handler-case
           (format-system-info (introspect-system system-name))
         (error (e)
           (format nil "Error: ~A" e))))))

  ;; system-dependencies: Get dependency graph
  (register-tool
   "system-dependencies"
   "Get the dependency graph for an ASDF system. Can show just direct dependencies or include all transitive dependencies."
   '(("type" . "object")
     ("required" . ("system"))
     ("properties" . (("system" . (("type" . "string")
                                   ("description" . "Name of the ASDF system")))
                      ("transitive" . (("type" . "boolean")
                                       ("description" . "Include all transitive dependencies (default: false)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let ((system-name (cdr (assoc "system" args :test #'string=)))
           (transitive (cdr (assoc "transitive" args :test #'string=))))
       (handler-case
           (format-system-dependencies
            (introspect-system-dependencies system-name :transitive transitive))
         (error (e)
           (format nil "Error: ~A" e))))))

  ;; list-local-systems: Find systems in local projects
  (register-tool
   "list-local-systems"
   "List all ASDF systems available locally (from Quicklisp local-projects and ASDF source registry). Shows system names and their .asd file locations."
   `(("type" . "object")
     ("properties" . ,(make-hash-table :test #'equal)))
   (lambda (args session)
     (declare (ignore args session))
     (handler-case
         (format-local-systems (introspect-local-systems))
       (error (e)
         (format nil "Error: ~A" e)))))

  ;; find-system-file: Locate a system's .asd file
  (register-tool
   "find-system-file"
   "Find the .asd file for an ASDF system. Returns the full path to the system definition file."
   '(("type" . "object")
     ("required" . ("system"))
     ("properties" . (("system" . (("type" . "string")
                                   ("description" . "Name of the ASDF system to locate"))))))
   (lambda (args session)
     (declare (ignore session))
     (let* ((system-name (cdr (assoc "system" args :test #'string=)))
            (result (introspect-find-system-file system-name)))
       (if (getf result :found)
           (format nil "System: ~A~%Location: ~A"
                   (getf result :name)
                   (getf result :pathname))
           (format nil "System ~A not found" system-name)))))

  ;; quickload: Load via Quicklisp
  (register-tool
   "quickload"
   "Load an ASDF system via Quicklisp. Will automatically download the system and its dependencies if not already installed. Safer than load-system for external dependencies."
   '(("type" . "object")
     ("required" . ("system"))
     ("properties" . (("system" . (("type" . "string")
                                   ("description" . "Name of the system to load")))
                      ("verbose" . (("type" . "boolean")
                                    ("description" . "Show detailed loading output (default: false)"))))))
   (lambda (args session)
     (let ((system-name (cdr (assoc "system" args :test #'string=)))
           (verbose (cdr (assoc "verbose" args :test #'string=))))
       (handler-case
           (progn
             (let ((result (introspect-quickload system-name :verbose verbose)))
               (when session
                 (push (getf result :system) (session-loaded-systems session)))
               (format-quickload-result result)))
         (error (e)
           (format nil "Error loading ~A: ~A" system-name e))))))

  ;; quicklisp-search: Search available systems
  (register-tool
   "quicklisp-search"
   "Search Quicklisp for available systems matching a pattern. Returns a list of system names that can be loaded with quickload."
   '(("type" . "object")
     ("required" . ("pattern"))
     ("properties" . (("pattern" . (("type" . "string")
                                    ("description" . "Search pattern (case-insensitive substring)")))
                      ("limit" . (("type" . "integer")
                                  ("description" . "Maximum number of results (default: 30)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let ((pattern (cdr (assoc "pattern" args :test #'string=)))
           (limit (or (cdr (assoc "limit" args :test #'string=)) 30)))
       (handler-case
           (format-quicklisp-search-results
            (introspect-quicklisp-search pattern :limit limit)
            pattern)
         (error (e)
           (format nil "Error: ~A" e))))))

  ;; load-file: Load a single Lisp file
  (register-tool
   "load-file"
   "Load a single Lisp file into the running image. Can optionally compile the file first. Use this for loading individual files that aren't part of an ASDF system."
   '(("type" . "object")
     ("required" . ("path"))
     ("properties" . (("path" . (("type" . "string")
                                 ("description" . "Path to the Lisp file to load")))
                      ("compile" . (("type" . "boolean")
                                    ("description" . "Compile the file before loading (default: false)")))
                      ("package" . (("type" . "string")
                                    ("description" . "Package context for loading (default: CL-USER)"))))))
   (lambda (args session)
     (declare (ignore session))
     (let ((path (cdr (assoc "path" args :test #'string=)))
           (compile (cdr (assoc "compile" args :test #'string=)))
           (package (or (cdr (assoc "package" args :test #'string=)) "CL-USER")))
       (handler-case
           (format-load-file-result
            (introspect-load-file path :compile compile :package package))
         (error (e)
           (format nil "Error loading file: ~A" e)))))))

;;; ==========================================================================
;;; Usage Guide Content
;;; ==========================================================================

(defun get-usage-guide-content ()
  "Return the usage guide for effective MCP server usage."
  "# CL-MCP-Server Usage Guide

## Quick Start

This server provides a persistent Common Lisp REPL accessible via MCP tools.
Definitions persist across calls within a session.

## Available Tools

| Tool | Purpose | When to Use |
|------|---------|-------------|
| evaluate-lisp | Execute code, persist definitions | Main development tool |
| validate-syntax | Check paren balance, syntax | BEFORE saving files |
| compile-form | Type check without execution | Pre-commit verification |
| describe-symbol | Inspect symbols | Understanding APIs |
| apropos-search | Find symbols by pattern | Discovering functions |
| macroexpand-form | Expand macros | Debug macro usage |
| time-execution | Profile with timing | Performance analysis |
| describe-last-error | Get last error details | After an error occurs |
| get-backtrace | Get error stack trace | Diagnosing errors |
| class-info | Inspect CLOS classes | Understanding class structure |
| find-methods | Find methods on a class | Discovering class operations |
| describe-system | Get ASDF system info | Before loading systems |
| system-dependencies | Get dependency graph | Understanding dependencies |
| list-local-systems | Find local systems | Discovering available systems |
| quickload | Load via Quicklisp | Loading external libraries |
| quicklisp-search | Search Quicklisp | Finding libraries |
| load-file | Load a Lisp file | Loading individual files |
| list-definitions | Show session state | Review what's defined |
| reset-session | Clear all state | Start fresh |

## Recommended Workflow

### 1. Incremental Development
Build up code piece by piece, testing as you go:

```
evaluate-lisp: (defun helper (x) (1+ x))     ; Define
evaluate-lisp: (helper 5)                     ; Test -> 6
evaluate-lisp: (defun main (lst) (mapcar #'helper lst))
evaluate-lisp: (main '(1 2 3))                ; Test -> (2 3 4)
```

### 2. Validate Before Save (CRITICAL)
ALWAYS validate syntax before writing Lisp files:

```
1. Prepare new file content
2. Call validate-syntax with full content
3. If valid: save file
4. If invalid: fix errors, repeat step 2
```

This prevents parenthesis mismatches that are hard to debug.

### 3. Explore Before Implementing
Use introspection to understand existing code:

```
apropos-search: pattern=\"hash\"       ; Find hash-related functions
describe-symbol: name=\"gethash\"      ; Understand the API
```

### 4. Type Check Before Commit
Use compile-form for thorough checking:

```
compile-form catches type errors that evaluate-lisp misses
```

## Common Patterns

### Define and Test
```lisp
evaluate-lisp: (defun factorial (n)
                 (if (<= n 1) 1 (* n (factorial (1- n)))))
evaluate-lisp: (mapcar #'factorial '(1 2 3 4 5))
```

### Capture Timing
```lisp
evaluate-lisp with capture-time=true for timing info
```

### Package Context
```lisp
evaluate-lisp with package=\"MY-PACKAGE\" for specific package context
```

## Anti-Patterns to Avoid

1. **Don't save without validation** - Always call validate-syntax first
2. **Don't write large untested code** - Build incrementally
3. **Don't guess APIs** - Use apropos-search and describe-symbol
4. **Don't ignore compile warnings** - Use compile-form

## Error Recovery

- Syntax errors: Use validate-syntax to find the issue
- Runtime errors: Server catches all errors, won't crash
- Lost state: Use list-definitions to see what's defined
- Fresh start: Use reset-session

## Session Persistence

- Functions, variables, macros persist across calls
- Loaded systems (via load-system) persist
- Package context can be set per-call

Call list-definitions to see current session state.")

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
