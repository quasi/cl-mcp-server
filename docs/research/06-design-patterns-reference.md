# Design Patterns Reference: Lessons from CL MCP Implementations

This document extracts strong design decisions and implementation strategies from the repositories we studied. Use as reference during planning and implementation.

---

## 1. Tool Definition Patterns

### 1.1 Declarative Tool Macro (40ants/mcp)

**Pattern**: Define tools declaratively with type annotations and automatic JSON Schema generation.

```lisp
;; From 40ants/mcp
(define-tool (example-tools echo) (text)
  (:summary "Returns input text back")
  (:param text string "Text to echo")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content :text text)))
```

**Why it's good**:
- Single source of truth for tool definition
- Type information generates JSON Schema automatically
- Reduces boilerplate vs manual registration
- Self-documenting

**Adoption notes**: Create a `deftool` macro that generates:
1. Handler function
2. JSON Schema for MCP
3. Tool registration
4. Parameter validation

---

### 1.2 Tool Categories and Namespacing (lisply-mcp)

**Pattern**: Prefix tool names to enable multiple backend namespaces.

```javascript
// Tool naming: backend__tool_name
"gendl-ccl__lisp_eval"
"gendl-ccl__http_request"
```

**Why it's good**:
- Supports multiple Lisp backends from one MCP server
- Clear provenance of each tool
- Avoids name collisions

**Adoption notes**: Consider prefixing if we add multiple tool categories (e.g., `introspection__describe-symbol`, `eval__evaluate-lisp`).

---

### 1.3 Tool Safety Levels (Agent-Q)

**Pattern**: Classify tools by their safety impact.

```lisp
;; From agent-q
(define-tool "describe_symbol"
  ...
  :safety-level :safe      ; Read-only, can't cause damage
  :categories '(:introspection)
  ...)

;; Safety levels:
;; :safe      - Read-only introspection (describe, apropos, who-calls)
;; :moderate  - State-changing but reversible (eval, compile)
;; :dangerous - Potentially destructive (delete-file, invoke-restart)
```

**Why it's good**:
- Enables safety-aware tool selection
- Documents risk level for users
- Could gate dangerous operations behind confirmation

**Adoption notes**: Add `:safety-level` to tool metadata. Display in `tools/list` response.

---

## 2. Error Handling Patterns

### 2.1 Structured Error Responses (cl-ai-project/cl-mcp)

**Pattern**: Return rich error data, not just message strings.

```lisp
;; Rich error structure
(:error t
 :type "UNDEFINED-FUNCTION"
 :message "The function FOO is undefined"
 :details (:name FOO :alternatives (FOO-BAR FOO-BAZ))
 :backtrace ((:frame 0 :function "EVAL" :source nil)
             (:frame 1 :function "MY-FUNCTION" :source "/path:42"))
 :restarts ((:name "CONTINUE" :description "Retry using FOO")
            (:name "USE-VALUE" :description "Use a different function")))
```

**Why it's good**:
- Enables intelligent error recovery by AI
- Provides actionable information
- Mirrors Sly debugger capabilities

**Adoption notes**: Capture condition type, format backtrace with source locations, list available restarts.

---

### 2.2 Parameter Validation Conditions (belyak/mcp-srv-lisp)

**Pattern**: Custom conditions for parameter validation errors.

```lisp
;; Custom conditions
(define-condition missing-required-parameter (mcp-error)
  ((parameter-name :initarg :parameter-name)))

(define-condition invalid-parameter-type (mcp-error)
  ((parameter-name :initarg :parameter-name)
   (expected-type :initarg :expected-type)
   (actual-value :initarg :actual-value)))
```

**Why it's good**:
- Precise error reporting
- Enables programmatic error handling
- Clear error messages for users

**Adoption notes**: Create condition hierarchy for validation errors. Return structured error responses.

---

### 2.3 Production Error Logging (lisply-mcp)

**Pattern**: Structured JSON logging for observability.

```javascript
// JSON log format
{
  "timestamp": "2026-01-25T10:30:00Z",
  "level": "error",
  "tool": "lisp_eval",
  "duration_ms": 150,
  "error": {
    "type": "evaluation-error",
    "message": "...",
    "backtrace": [...]
  }
}
```

**Why it's good**:
- Machine-parseable for log aggregation
- Includes timing for performance analysis
- Structured for filtering/searching

**Adoption notes**: Log to stderr in JSON format. Include tool name, duration, result type.

---

## 3. Transport Patterns

### 3.1 Transport Abstraction (40ants/mcp)

**Pattern**: Abstract transport behind generic functions.

```lisp
;; Base protocol
(defgeneric start-loop (transport message-handler))
(defgeneric stop-loop (transport))
(defgeneric receive-message (transport))
(defgeneric send-message (transport message))

;; Implementations
(defclass stdio-transport () ...)
(defclass http-transport () ...)
```

**Why it's good**:
- Easy to add new transports without changing protocol code
- Testable (can mock transport)
- Clean separation of concerns

**Adoption notes**: Our current transport is hard-coded stdio. If we ever need HTTP/SSE, this pattern enables it cleanly.

---

### 3.2 Clean STDIO (xbill999 article)

**Pattern**: Ensure stdout is reserved for MCP protocol only.

```lisp
;; Redirect all logging to stderr
(setf *error-output* *standard-output*)
(setf *trace-output* *error-output*)

;; Or use a logging framework with stderr destination
(log:config :sane2 :stream *error-output*)
```

**Why it's good**:
- Prevents logging from corrupting JSON-RPC stream
- Essential for stdio transport reliability
- Debug output still available

**Adoption notes**: Ensure all debug/trace output goes to stderr. Test by enabling verbose logging and verifying MCP still works.

---

## 4. Security Patterns

### 4.1 Project-Scoped File Access (cl-ai-project/cl-mcp)

**Pattern**: Restrict file operations to project root + registered paths.

```lisp
(defvar *project-root* nil)
(defvar *allowed-paths* '())  ; Additional allowed paths (ASDF systems, etc.)

(defun allowed-read-path-p (path)
  "Check if PATH is within allowed boundaries"
  (let ((resolved (truename path)))
    (or (path-prefix-p *project-root* resolved)
        (some (lambda (allowed) (path-prefix-p allowed resolved))
              *allowed-paths*))))

(defun ensure-write-path (path)
  "Signal error if PATH is not writable"
  (unless (and (allowed-read-path-p path)
               (path-prefix-p *project-root* (truename path)))
    (error 'path-access-denied :path path)))
```

**Why it's good**:
- Prevents accidental writes outside project
- Allows read access to dependencies (ASDF systems)
- Clear security boundary

**Adoption notes**: Not needed for file tools (Claude has those). But useful if we add `load-file` to restrict what can be loaded into the image.

---

### 4.2 Hidden File Filtering (cl-ai-project/cl-mcp)

**Pattern**: Exclude sensitive files from directory listings.

```lisp
(defparameter *hidden-patterns*
  '(".git" ".svn" ".hg"           ; VCS
    ".cache" "__pycache__"        ; Caches
    "*.fasl" "*.dx64fsl"          ; Compiled
    ".env" "*.secret"))           ; Secrets

(defun visible-file-p (path)
  (not (some (lambda (pattern)
               (pathname-match-p path pattern))
             *hidden-patterns*)))
```

**Why it's good**:
- Reduces noise in listings
- Prevents exposing secrets
- Keeps focus on source code

**Adoption notes**: Useful for `list-local-systems` or any tool that enumerates files.

---

## 5. Introspection Patterns

### 5.1 sb-introspect Integration (Agent-Q, cl-ai-project)

**Pattern**: Use SBCL's introspection package for rich symbol information.

```lisp
;; Function information
(sb-introspect:function-lambda-list #'my-function)
;; => (X Y &OPTIONAL Z)

;; Cross-references
(sb-introspect:who-calls 'my-function)
;; => ((CALLER-1 . #<SB-INTROSPECT:DEFINITION-SOURCE ...>)
;;    (CALLER-2 . #<SB-INTROSPECT:DEFINITION-SOURCE ...>))

;; Source location
(sb-introspect:find-definition-source #'my-function)
;; => #<SB-INTROSPECT:DEFINITION-SOURCE {pathname: "/path/to/file.lisp", ...}>

;; Type information for variables
(sb-introspect:who-references 'my-variable)
(sb-introspect:who-sets 'my-variable)
(sb-introspect:who-binds 'my-variable)
```

**Why it's good**:
- Accurate information from the running image
- Source locations for navigation
- Cross-reference for impact analysis

**Adoption notes**: Core of Phase A introspection tools. Wrap in helper functions that format results consistently.

---

### 5.2 CLOS Introspection (Agent-Q)

**Pattern**: Use MOP for class/method information.

```lisp
;; Using closer-mop for portability (or direct MOP for SBCL-only)
(defun class-slot-info (class-name)
  (let ((class (find-class class-name)))
    (mapcar (lambda (slot)
              (list :name (closer-mop:slot-definition-name slot)
                    :type (closer-mop:slot-definition-type slot)
                    :initarg (first (closer-mop:slot-definition-initargs slot))
                    :initform (closer-mop:slot-definition-initform slot)))
            (closer-mop:class-slots class))))

(defun class-methods (class-name)
  (let ((class (find-class class-name)))
    (mapcar (lambda (method)
              (list :name (closer-mop:generic-function-name
                           (closer-mop:method-generic-function method))
                    :qualifiers (closer-mop:method-qualifiers method)
                    :specializers (mapcar #'class-name
                                          (closer-mop:method-specializers method))))
            (closer-mop:specializer-direct-methods class))))
```

**Why it's good**:
- Enables understanding of OO code
- Class hierarchy visualization
- Method discovery

**Adoption notes**: Phase D tools. Can use SBCL's MOP directly since we're SBCL-only.

---

## 6. Session Management Patterns

### 6.1 Definition Tracking (cl-mcp-server - our own)

**Pattern**: Track what gets defined during the session.

```lisp
(defvar *session-definitions*
  '(:functions ()
    :variables ()
    :macros ()
    :classes ()
    :packages ()))

(defun track-definition (type name)
  (pushnew name (getf *session-definitions* type)))

;; Detect definitions in evaluated code
(defun detect-definitions (form)
  (when (consp form)
    (case (first form)
      (defun (track-definition :functions (second form)))
      (defvar (track-definition :variables (second form)))
      (defparameter (track-definition :variables (second form)))
      (defmacro (track-definition :macros (second form)))
      (defclass (track-definition :classes (second form)))
      (defpackage (track-definition :packages (second form))))))
```

**Why it's good**:
- Session introspection (what did I define?)
- Clean reset (remove only session definitions)
- Debugging aid

**Adoption notes**: We already have this. Enhance with timestamps and source tracking.

---

### 6.2 Package Context (cl-ai-project/cl-mcp)

**Pattern**: Track and allow setting the current package for evaluation.

```lisp
(defvar *session-package* (find-package :cl-user))

(defun evaluate-in-package (code package-name)
  (let ((*package* (or (find-package package-name)
                       (error "Package ~A not found" package-name))))
    (eval (read-from-string code))))

;; Tool parameter
(deftool evaluate-lisp
  "Evaluate code"
  (:param code :string :required t)
  (:param package :string :doc "Package context (default: CL-USER)")
  ...)
```

**Why it's good**:
- Proper symbol resolution
- Works with multi-package projects
- Matches REPL behavior

**Adoption notes**: Add `:package` parameter to `evaluate-lisp`. Track current package in session state.

---

## 7. ASDF Integration Patterns

### 7.1 System Discovery (cl-ai-project/cl-mcp)

**Pattern**: Find and list available ASDF systems.

```lisp
(defun list-local-systems ()
  "Find .asd files in current directory tree"
  (let ((systems '()))
    (uiop:collect-sub*directories
     (uiop:getcwd)
     (constantly t)
     (constantly t)
     (lambda (dir)
       (dolist (asd (directory (merge-pathnames "*.asd" dir)))
         (push (list :name (pathname-name asd)
                     :pathname (namestring asd))
               systems))))
    systems))

(defun system-info (system-name)
  "Get detailed system information"
  (let ((system (asdf:find-system system-name)))
    (when system
      (list :name (asdf:component-name system)
            :version (asdf:component-version system)
            :description (asdf:system-description system)
            :author (asdf:system-author system)
            :license (asdf:system-license system)
            :depends-on (asdf:system-depends-on system)
            :pathname (asdf:system-source-directory system)))))
```

**Why it's good**:
- Understand project structure
- Dependency awareness
- Enable loading project systems

**Adoption notes**: Core of Phase E. Add `list-local-systems`, `describe-system`, `system-dependencies` tools.

---

### 7.2 Load with Feedback (enhancement)

**Pattern**: Return useful information when loading systems.

```lisp
(defun load-system-with-feedback (system-name)
  "Load system and return what happened"
  (let ((warnings '())
        (start-time (get-internal-real-time))
        (initial-definitions (copy-list *session-definitions*)))
    (handler-bind
        ((warning (lambda (w)
                    (push (princ-to-string w) warnings)
                    (muffle-warning w))))
      (asdf:load-system system-name))
    (let ((end-time (get-internal-real-time)))
      (list :loaded t
            :system system-name
            :load-time-ms (round (* 1000 (/ (- end-time start-time)
                                            internal-time-units-per-second)))
            :warnings (nreverse warnings)
            :new-definitions (diff-definitions initial-definitions
                                               *session-definitions*)))))
```

**Why it's good**:
- Confirmation of success
- Timing for performance awareness
- Track what got defined

**Adoption notes**: Enhance existing `load-system` tool with this pattern.

---

## 8. Evaluation Patterns

### 8.1 Output Capture (cl-mcp-server - our own)

**Pattern**: Capture all output streams during evaluation.

```lisp
(defun evaluate-with-capture (code)
  "Evaluate code, capturing stdout, stderr, and warnings"
  (let ((stdout-capture (make-string-output-stream))
        (warnings '()))
    (handler-bind
        ((warning (lambda (w)
                    (push (princ-to-string w) warnings)
                    (muffle-warning w))))
      (let ((*standard-output* stdout-capture)
            (*error-output* stdout-capture)
            (*trace-output* stdout-capture))
        (multiple-value-list (eval (read-from-string code)))))
    (values result
            (get-output-stream-string stdout-capture)
            (nreverse warnings))))
```

**Why it's good**:
- See what code prints
- Capture warnings without stopping
- Complete picture of execution

**Adoption notes**: We already have this. Consider separating stdout/stderr in output.

---

### 8.2 Multiple Value Handling (standard pattern)

**Pattern**: Return all values, not just the first.

```lisp
(defun format-result (values)
  "Format multiple values for return"
  (if (= 1 (length values))
      (format nil "~S" (first values))
      (format nil "~{~S~^~%~}" values)))

;; Or as structured data
(defun result-to-plist (values)
  (list :values (mapcar #'prin1-to-string values)
        :primary-value (prin1-to-string (first values))
        :value-count (length values)))
```

**Why it's good**:
- Lisp functions often return multiple values
- Important information in secondary values
- Matches REPL behavior

**Adoption notes**: Ensure `evaluate-lisp` returns all values, clearly labeled.

---

## 9. Timeout and Safety Patterns

### 9.1 Evaluation Timeout (from post-mortem analysis)

**Pattern**: Wrap evaluations with configurable timeout.

```lisp
(defparameter *evaluation-timeout* 30) ; seconds

(defun evaluate-with-timeout (form)
  (handler-case
      (bt:with-timeout (*evaluation-timeout*)
        (eval form))
    (bt:timeout ()
      (error 'evaluation-timeout
             :seconds *evaluation-timeout*
             :form form))))
```

**Why it's good**:
- Prevents infinite loops from hanging server
- Recoverable failure mode
- Configurable per-use-case

**Adoption notes**: Phase 0 priority. Add `bt:with-timeout` wrapper to all evaluations.

---

### 9.2 Stack Capture on Error (enhancement)

**Pattern**: Capture stack trace when errors occur.

```lisp
(defun capture-backtrace ()
  "Capture current stack as structured data"
  (let ((frames '()))
    (sb-debug:map-backtrace
     (lambda (frame)
       (push (list :function (sb-di:debug-fun-name
                               (sb-di:frame-debug-fun frame))
                   :source (sb-di:debug-source-namestring
                             (sb-di:code-location-debug-source
                               (sb-di:frame-code-location frame))))
             frames)))
    (nreverse frames)))
```

**Why it's good**:
- Know what was happening when error occurred
- Debugging without interactive debugger
- Structured for programmatic analysis

**Adoption notes**: Use in error handler. Format for MCP response.

---

## 10. Code Quality Patterns

### 10.1 Condition Hierarchy (belyak/mcp-srv-lisp)

**Pattern**: Organize conditions in a hierarchy.

```lisp
(define-condition mcp-condition (condition) ())

(define-condition mcp-error (mcp-condition error) ())
(define-condition mcp-warning (mcp-condition warning) ())

(define-condition protocol-error (mcp-error)
  ((code :initarg :code :reader error-code)))

(define-condition tool-error (mcp-error)
  ((tool-name :initarg :tool-name :reader error-tool-name)))

(define-condition validation-error (tool-error)
  ((parameter :initarg :parameter :reader error-parameter)))
```

**Why it's good**:
- Handler-case can be precise or general
- Clear error categorization
- Extensible

**Adoption notes**: Define condition hierarchy. All our errors should inherit from `mcp-error`.

---

### 10.2 Factory Functions (belyak/mcp-srv-lisp)

**Pattern**: Use factory functions for object creation.

```lisp
(defvar *request-factory* #'make-standard-request)

(defun make-request (type &rest args)
  (apply *request-factory* type args))

;; Can be overridden for testing
(let ((*request-factory* #'make-mock-request))
  (run-tests))
```

**Why it's good**:
- Testability (inject mocks)
- Extensibility (customize creation)
- Centralized construction logic

**Adoption notes**: Consider for tool/response creation if we need testability.

---

## Summary: Key Patterns to Adopt

| Priority | Pattern | Source | Phase |
|----------|---------|--------|-------|
| **P0** | Evaluation timeout | Post-mortem | 0 |
| **P0** | Stack capture on error | Enhancement | 0 |
| **P1** | Declarative tool macro | 40ants/mcp | A |
| **P1** | sb-introspect integration | Agent-Q | A |
| **P1** | Structured error responses | cl-ai-project | C |
| **P1** | Tool safety levels | Agent-Q | A |
| **P2** | Package-aware evaluation | cl-ai-project | B |
| **P2** | System discovery | cl-ai-project | E |
| **P2** | Load with feedback | Enhancement | E |
| **P2** | CLOS introspection | Agent-Q | D |
| **P3** | Transport abstraction | 40ants/mcp | Future |
| **P3** | Hidden file filtering | cl-ai-project | E |
| **P3** | Condition hierarchy | belyak | Ongoing |

---

## References

- belyak/mcp-srv-lisp: https://github.com/belyak/mcp-srv-lisp
- gornskew/lisply-mcp: https://github.com/gornskew/lisply-mcp
- 40ants/mcp: https://github.com/40ants/mcp
- cl-ai-project/cl-mcp: https://github.com/cl-ai-project/cl-mcp
- Agent-Q: /Users/quasi/quasilabs/projects/agent-q
- SBCL Manual Chapter 13: Introspection
- MOP: closer-mop or SBCL's sb-mop
