# Implementation Roadmap: Claude-Optimized CL Development Server

## Vision Statement

**cl-mcp-server** becomes the premier tool for Claude to do Common Lisp development by providing what Claude cannot get elsewhere: **direct access to a live SBCL image** with rich introspection, evaluation, and error intelligence.

---

## Strategic Position

```
                    Full MCP Compliance
                         │
                         │ belyak/mcp-srv-lisp
                         │
    Framework-First ─────┼───────────────── Agent-First
                         │
    40ants/mcp           │     ★ cl-mcp-server (target)
                         │
                    Live Image Power
```

**Our niche**: Not competing on MCP feature breadth. Competing on **depth of Lisp image access** for AI agents.

---

## Implementation Phases

### Phase 0: Robustness & Safety (PRIORITY)

**Goal**: Prevent server hangs from becoming unrecoverable. Based on real-world crash analysis from todo-api development.

**Problem Statement**: When code hangs (infinite recursion, infinite loops, long-running operations), the MCP server becomes completely unresponsive with:
- No error message returned
- No way to interrupt
- Have to kill and restart the server
- Loss of all REPL state (loaded systems, defined functions)

#### 0.1 Evaluation Timeout (Critical)

```lisp
(defparameter *evaluation-timeout* 30) ; seconds, configurable

;; Wrap all evaluations with timeout
(defun evaluate-with-timeout (form)
  (handler-case
    (bt:with-timeout (*evaluation-timeout*)
      (eval form))
    (bt:timeout ()
      (make-error-result
        :type "EVALUATION-TIMEOUT"
        :message (format nil "Evaluation exceeded ~A seconds" *evaluation-timeout*)
        :hint "Consider breaking into smaller operations or increasing timeout"))))
```

#### 0.2 Interrupt Capability

```lisp
;; New MCP tool: interrupt-evaluation
(deftool interrupt-evaluation
  "Interrupt a long-running evaluation"
  (:result (:interrupted t :message "Evaluation interrupted")))

;; Implementation requires threading:
;; - Evaluations run in separate thread
;; - Main thread can signal interrupt
;; - bt:interrupt-thread or similar mechanism
```

#### 0.3 Heartbeat During Long Operations

```lisp
;; For operations expected to be long, send periodic heartbeats
;; MCP protocol allows notifications - use them

(defun evaluate-with-heartbeat (form)
  (let ((eval-thread (bt:make-thread
                       (lambda () (eval form)))))
    (loop while (bt:thread-alive-p eval-thread)
          do (send-heartbeat-notification)
             (sleep 5))
    (bt:join-thread eval-thread)))
```

#### 0.4 Stack Trace on Timeout

```lisp
;; When timeout occurs, capture what was happening
(handler-case
  (bt:with-timeout (*evaluation-timeout*)
    (eval form))
  (bt:timeout ()
    (let ((stack (capture-current-stack eval-thread)))
      (make-error-result
        :type "EVALUATION-TIMEOUT"
        :message "Evaluation timed out"
        :backtrace stack  ; What was the code doing?
        :hint "Possible infinite loop or expensive operation"))))
```

#### 0.5 Configurable Limits

```lisp
;; Expose configuration via MCP tool
(deftool configure-limits
  "Configure evaluation safety limits"
  (:param timeout :integer :doc "Evaluation timeout in seconds (default: 30)")
  (:param max-output :integer :doc "Max output characters (default: 100000)")
  (:result (:timeout 30 :max-output 100000)))
```

#### 0.6 Graceful Degradation

When things go wrong, preserve what we can:

```lisp
;; On unrecoverable error, dump session state
(defun emergency-session-dump ()
  "Called before crash - save what we can"
  (with-open-file (out "/tmp/cl-mcp-session-dump.lisp" :direction :output)
    (format out ";; Emergency session dump at ~A~%" (get-universal-time))
    (format out ";; Package: ~A~%" (package-name *package*))
    (dump-session-definitions out)))
```

#### Implementation Priority

| Feature | Complexity | Impact | Priority |
|---------|-----------|--------|----------|
| Evaluation timeout | Low | Critical | P0 |
| Stack trace on timeout | Medium | High | P0 |
| Configurable limits | Low | Medium | P1 |
| Interrupt capability | High | High | P1 |
| Heartbeat notifications | Medium | Medium | P2 |
| Session dump on crash | Medium | Medium | P2 |

**Dependencies**: Requires `bordeaux-threads` (already common in CL ecosystem).

---

### Phase A: Core Introspection (2-3 weeks)

**Goal**: Let Claude "see" the running image as clearly as a human with Sly.

#### A.1 Symbol Information Tools

```lisp
;; describe-symbol
(deftool describe-symbol
  "Get comprehensive information about a symbol"
  (:param name :string :required t :doc "Symbol name")
  (:param package :string :doc "Package (default: current)")
  (:result (:type :function|:macro|:generic|:variable|:class|:symbol
            :package "package-name"
            :value "if bound"
            :documentation "docstring"
            :arglist (args) ;; for functions
            :source-file "/path"
            :source-line 42)))
```

**Implementation approach**:
```lisp
(defun tool-describe-symbol (name &optional package)
  (let* ((pkg (or (and package (find-package package)) *package*))
         (sym (find-symbol (string-upcase name) pkg)))
    (when sym
      (list :name (symbol-name sym)
            :package (package-name (symbol-package sym))
            :type (symbol-type sym)
            :documentation (documentation sym 'function)
            :arglist (when (fboundp sym)
                       (sb-introspect:function-lambda-list sym))
            :source (sb-introspect:find-definition-source sym)))))
```

#### A.2 Search/Discovery Tools

```lisp
;; apropos-search
(deftool apropos-search
  "Find symbols matching a pattern"
  (:param pattern :string :required t :doc "Search pattern (substring)")
  (:param package :string :doc "Limit to package (optional)")
  (:param type :string :doc "Filter by type: function|variable|class")
  (:result ((:name "sym" :package "pkg" :type :function) ...)))
```

#### A.3 Cross-Reference Tools

```lisp
;; who-calls - SBCL specific
(deftool who-calls
  "Find all functions that call the specified function"
  (:param name :string :required t :doc "Function name")
  (:param package :string :doc "Package")
  (:result ((:caller "name" :package "pkg" :location "/path:line") ...)))

;; Implementation using sb-introspect
(defun tool-who-calls (name &optional package)
  (let* ((sym (resolve-symbol name package))
         (callers (sb-introspect:who-calls sym)))
    (mapcar (lambda (caller)
              (list :caller (symbol-name (first caller))
                    :package (package-name (symbol-package (first caller)))
                    :location (format-source-location (second caller))))
            callers)))
```

#### A.4 Macro Understanding

```lisp
;; macroexpand-form
(deftool macroexpand-form
  "Expand macros to understand what code does"
  (:param form :string :required t :doc "Lisp form as string")
  (:param full :boolean :doc "Full expansion (default: one step)")
  (:param package :string :doc "Package context")
  (:result (:original "(form)" :expanded "(expanded)")))
```

---

### Phase B: Enhanced Evaluation (2-3 weeks)

**Goal**: Improve the evaluation experience beyond basic REPL.

#### B.1 Package-Aware Evaluation

```lisp
;; evaluate-lisp enhancement
(deftool evaluate-lisp
  "Evaluate Common Lisp code with enhanced feedback"
  (:param code :string :required t :doc "Code to evaluate")
  (:param package :string :doc "Package context (default: CL-USER)")
  (:param capture-output :boolean :doc "Capture *standard-output*")
  (:param capture-time :boolean :doc "Include timing information")
  (:result (:values (val1 val2 ...)
            :output "printed output"
            :warnings (...)
            :time (:real-ms 5 :gc-ms 0)
            :new-definitions (:functions (foo bar) :variables (x)))))
```

#### B.2 Compilation Tool

```lisp
;; compile-form
(deftool compile-form
  "Compile code (not just eval) to catch compilation errors"
  (:param code :string :required t)
  (:param package :string)
  (:result (:compiled t|nil
            :warnings (...)
            :errors (...))))
```

#### B.3 Profiling Tool

```lisp
;; time-execution
(deftool time-execution
  "Execute with timing and allocation info"
  (:param code :string :required t)
  (:result (:result "..."
            :real-time-ms 5
            :run-time-ms 4
            :gc-time-ms 0
            :bytes-allocated 1024
            :page-faults 0)))
```

---

### Phase C: Error Intelligence (2 weeks)

**Goal**: Give Claude the same error understanding a human debugger provides.

#### C.1 Rich Error Reporting

Current error handling returns strings. Enhance to return structured data:

```lisp
(:error t
 :type "UNDEFINED-FUNCTION"
 :message "The function FOO is undefined"
 :details (:name FOO :alternatives (FOO-BAR FOO-BAZ))
 :backtrace ((:frame 0 :function "EVAL" :source nil)
             (:frame 1 :function "MY-FUNCTION" :source "/path:42")
             ...)
 :restarts ((:name "CONTINUE" :description "Retry using FOO")
            (:name "USE-VALUE" :description "Use a different function")
            (:name "ABORT" :description "Abort evaluation")))
```

#### C.2 Condition Description Tool

```lisp
;; describe-last-error
(deftool describe-last-error
  "Get detailed information about the most recent error"
  (:result (:type "..." :message "..." :backtrace (...) :restarts (...))))
```

#### C.3 Backtrace Tool

```lisp
;; get-backtrace
(deftool get-backtrace
  "Get stack trace with local variables"
  (:param max-frames :integer :doc "Max frames to return (default: 20)")
  (:result ((:frame 0 :function "name" :locals ((x . 5) (y . "hello"))} ...)))
```

---

### Phase D: CLOS Intelligence (1-2 weeks)

**Goal**: Let Claude understand object-oriented Lisp code.

#### D.1 Class Structure

```lisp
;; class-info
(deftool class-info
  "Get complete class information including slots and methods"
  (:param class :string :required t)
  (:result (:name "..."
            :superclasses (...)
            :slots ((:name slot1 :type T :initarg :slot1 :initform nil) ...)
            :direct-methods ((:name method1 :qualifiers nil :specializers (...)) ...))))
```

#### D.2 Method Discovery

```lisp
;; find-methods
(deftool find-methods
  "Find all methods specialized on a class"
  (:param class :string :required t)
  (:result ((:generic "gf-name" :qualifiers (...) :lambda-list (...)) ...)))
```

---

### Phase E: ASDF & Quicklisp Integration (2-3 weeks)

**Goal**: Load and understand project structure. Claude reads source files directly; MCP loads them into the image.

#### E.1 System Information

```lisp
;; describe-system
(deftool describe-system
  "Get ASDF system information"
  (:param system :string :required t)
  (:result (:name "..."
            :version "..."
            :description "..."
            :author "..."
            :license "..."
            :pathname "/path/to/system.asd"
            :components ((:name "foo" :type :file :pathname "src/foo.lisp") ...)
            :dependencies ("alexandria" "cl-ppcre" ...))))
```

#### E.2 Enhanced Load System

Already have `load-system`. Enhance with:
- Dependency resolution feedback
- Compilation warnings capture
- Load time tracking
- New definitions tracking (what got defined)

```lisp
;; Enhanced load-system result
(:loaded t
 :system "my-system"
 :load-time-ms 1250
 :dependencies-loaded ("alexandria" "cl-ppcre")
 :warnings ((:file "src/foo.lisp" :message "...") ...)
 :new-definitions (:functions (foo bar) :classes (my-class) :packages (my-pkg)))
```

#### E.3 Quicklisp Integration

```lisp
;; quickload
(deftool quickload
  "Load system via Quicklisp (downloads if needed)"
  (:param system :string :required t)
  (:result (:loaded t
            :system "..."
            :downloaded nil  ; or list of newly downloaded systems
            :dependencies-loaded (...))))

;; quicklisp-search
(deftool quicklisp-search
  "Search Quicklisp for available systems"
  (:param term :string :required t)
  (:result ((:name "system-name" :description "..." :version "...") ...)))
```

#### E.4 System Dependencies

```lisp
;; system-dependencies
(deftool system-dependencies
  "Get dependency graph for a system"
  (:param system :string :required t)
  (:param transitive :boolean :doc "Include transitive deps")
  (:result ((:system "name" :depends-on (...)) ...)))
```

#### E.5 Local System Discovery

```lisp
;; list-local-systems
(deftool list-local-systems
  "List ASDF systems findable from current directory"
  (:result ((:name "system" :pathname "/path/to/system.asd") ...)))

;; find-system-file
(deftool find-system-file
  "Find the .asd file for a system"
  (:param system :string :required t)
  (:result (:found t :pathname "/path/to/system.asd")))
```

#### E.6 Load File

```lisp
;; load-file
(deftool load-file
  "Load a single Lisp file into the image"
  (:param path :string :required t :doc "Path to .lisp file")
  (:param compile :boolean :doc "Compile before loading (default: t)")
  (:result (:loaded t
            :pathname "/path/to/file.lisp"
            :warnings (...)
            :new-definitions (...))))
```

---

## Tool Definition Macro

**Problem**: Current tool registration is verbose.

**Solution**: `deftool` macro (from 40ants pattern):

```lisp
(defmacro deftool (name docstring &body params-and-body)
  "Define an MCP tool with cleaner syntax.

   Example:
   (deftool describe-symbol
     \"Get info about a symbol\"
     (:param name :string :required t :doc \"Symbol name\")
     (:param package :string :doc \"Package name\")
     (let* ((sym (resolve-symbol name package)))
       (describe-symbol-impl sym)))"
  ...)
```

This should generate:
1. JSON schema for MCP
2. Handler function
3. Registration call
4. Automatic parameter validation

---

## Testing Strategy

### Unit Tests

For each tool, test:
1. Normal operation with typical inputs
2. Edge cases (nil package, unknown symbol, etc.)
3. Error conditions (invalid input types)

### Integration Tests

Full MCP message round-trips:
1. `tools/list` returns all tools
2. `tools/call` with valid params succeeds
3. `tools/call` with invalid params returns proper error

### Regression Tests

Compare tool output against Sly/SLYNK for same queries:
```lisp
(deftest describe-matches-sly ()
  (let ((mcp-result (tool-describe-symbol "mapcar" "CL"))
        (sly-result (slynk:describe-symbol 'mapcar)))
    (assert-equal (getf mcp-result :arglist)
                  (getf sly-result :arglist))))
```

---

## Migration Path

### Current State

```
cl-mcp-server/
├── src/
│   ├── conditions.lisp      ; Error conditions
│   ├── evaluator.lisp       ; Code evaluation
│   ├── json-rpc.lisp        ; JSON-RPC protocol
│   ├── server.lisp          ; MCP server
│   ├── session.lisp         ; Session state
│   ├── tools.lisp           ; Tool definitions
│   └── transport.lisp       ; STDIO transport
```

### Target State

```
cl-mcp-server/
├── src/
│   ├── core/
│   │   ├── conditions.lisp
│   │   ├── json-rpc.lisp
│   │   ├── server.lisp
│   │   └── transport.lisp
│   ├── session/
│   │   ├── session.lisp
│   │   └── definitions.lisp  ; Enhanced definition tracking
│   ├── evaluation/
│   │   ├── evaluator.lisp
│   │   └── compiler.lisp     ; NEW: compile-form
│   ├── introspection/        ; NEW
│   │   ├── symbols.lisp      ; describe, apropos
│   │   ├── xref.lisp         ; who-calls, who-references
│   │   ├── clos.lisp         ; class-info, find-methods
│   │   └── packages.lisp     ; package inspection
│   ├── errors/               ; NEW
│   │   ├── capture.lisp      ; Rich error capture
│   │   └── reporting.lisp    ; Structured error output
│   └── tools/
│       ├── registry.lisp     ; Tool registry
│       ├── deftool.lisp      ; NEW: deftool macro
│       └── standard.lisp     ; Built-in tools
```

---

## Success Metrics

### Quantitative

| Metric | Target |
|--------|--------|
| Tool count | 15+ (up from 4) |
| Test coverage | >80% |
| Error information completeness | Matches Sly output |
| Response latency | <100ms for introspection |

### Qualitative

1. Claude can understand existing codebase by querying the image
2. Claude can debug errors without human stack trace interpretation
3. Claude can discover APIs via apropos and class inspection
4. Definition tracking helps Claude understand session evolution

---

## Division of Labor: Claude vs. MCP Server

| Capability | Handled By | Rationale |
|------------|-----------|-----------|
| Read source files | **Claude** (Read tool) | Native capability |
| Write source files | **Claude** (Write/Edit) | Native capability |
| Search files | **Claude** (Grep/Glob) | Native capability |
| Run shell commands | **Claude** (Bash) | Native capability |
| **Evaluate code** | **MCP Server** | Requires live image |
| **Introspect symbols** | **MCP Server** | Requires live image |
| **Load systems** | **MCP Server** | Requires live image |
| **Query callers/refs** | **MCP Server** | Requires live image |
| **Inspect errors** | **MCP Server** | Requires live image |
| **CLOS inspection** | **MCP Server** | Requires live image |

**Key insight**: The MCP server is a "truth oracle" for the running Lisp image. It does not duplicate Claude's native file system capabilities.

---

## Non-Goals (Explicit)

1. **Full MCP compliance** - Resources and prompts not needed
2. **HTTP transport** - STDIO is sufficient for local use
3. **Multi-implementation support** - SBCL-only is fine
4. **File system tools** - Claude already has file access (Read, Write, Edit, Grep, Glob)
5. **Sandboxing** - Trust model is explicit; Claude is trusted
6. **File content via MCP Resources** - Claude reads files directly

---

## Timeline Summary

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| **0: Robustness** | **1 week** | **timeout, interrupt, stack trace on hang** |
| A: Core Introspection | 2-3 weeks | describe-symbol, apropos-search, who-calls, macroexpand-form |
| B: Enhanced Evaluation | 2-3 weeks | compile-form, time-execution, package-aware eval |
| C: Error Intelligence | 2 weeks | structured errors, get-backtrace, restarts |
| D: CLOS Intelligence | 1-2 weeks | class-info, find-methods |
| E: ASDF & Quicklisp | 2-3 weeks | quickload, describe-system, system-dependencies, load-file |
| **Total** | **10-14 weeks** | **20+ tools, Claude-optimized** |

---

## Immediate Next Steps

### Phase 0 (Safety - Do First)

1. **Add evaluation timeout** - Wrap all evals with `bt:with-timeout`, default 30s
2. **Capture stack on timeout** - When timeout fires, grab the stack trace
3. **Add `configure-limits` tool** - Let Claude adjust timeout if needed

### Phase A (Introspection - After Safety)

4. **Create `deftool` macro** - Reduce boilerplate for new tools
5. **Implement `describe-symbol`** - Foundation introspection tool
6. **Implement `apropos-search`** - Discovery tool
7. **Add `who-calls`** - Cross-reference tool

---

## References

- [04-sly-lessons-for-mcp.md](04-sly-lessons-for-mcp.md) - Sly architecture analysis
- [03-recommendations.md](03-recommendations.md) - Competitor analysis recommendations
- [todo-api post-mortem](https://github.com/quasi/test-quick-api-project/blob/main/docs/software-development-practices.md) - Real-world crash analysis (Phase 0 source)
- Agent-Q introspection: `/Users/quasi/quasilabs/projects/agent-q/src/tools/introspection.lisp`
- SBCL Manual Chapter 13: Introspection
- bordeaux-threads: Threading library for timeout/interrupt implementation
