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

### Phase E: Project Integration (2-3 weeks)

**Goal**: Understand and work with ASDF projects.

#### E.1 System Information

```lisp
;; describe-system
(deftool describe-system
  "Get ASDF system information"
  (:param system :string :required t)
  (:result (:name "..."
            :version "..."
            :description "..."
            :components (...)
            :dependencies (...))))
```

#### E.2 Load System

Already have `load-system`. Enhance with:
- Dependency resolution feedback
- Compilation warnings capture
- Load time tracking

#### E.3 System Dependencies

```lisp
;; system-dependencies
(deftool system-dependencies
  "Get dependency graph for a system"
  (:param system :string :required t)
  (:param transitive :boolean :doc "Include transitive deps")
  (:result ((:system "name" :depends-on (...)) ...)))
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

## Non-Goals (Explicit)

1. **Full MCP compliance** - Resources and prompts not needed
2. **HTTP transport** - STDIO is sufficient for local use
3. **Multi-implementation support** - SBCL-only is fine
4. **File system tools** - Claude already has file access
5. **Sandboxing** - Trust model is explicit; Claude is trusted

---

## Timeline Summary

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| A: Core Introspection | 2-3 weeks | describe, apropos, who-calls, macroexpand |
| B: Enhanced Evaluation | 2-3 weeks | compile-form, time-execution, enhanced eval |
| C: Error Intelligence | 2 weeks | structured errors, backtrace, restarts |
| D: CLOS Intelligence | 1-2 weeks | class-info, find-methods |
| E: Project Integration | 2-3 weeks | describe-system, dependencies |
| **Total** | **9-13 weeks** | **15+ tools, Claude-optimized** |

---

## Immediate Next Steps

1. **Create `deftool` macro** - Reduce boilerplate for new tools
2. **Implement `describe-symbol`** - Foundation tool
3. **Implement `apropos-search`** - Discovery tool
4. **Enhance error reporting** - Structured errors first
5. **Add `who-calls`** - Most requested introspection feature

---

## References

- [04-sly-lessons-for-mcp.md](04-sly-lessons-for-mcp.md) - Sly architecture analysis
- [03-recommendations.md](03-recommendations.md) - Competitor analysis recommendations
- Agent-Q introspection: `/Users/quasi/quasilabs/projects/agent-q/src/tools/introspection.lisp`
- SBCL Manual Chapter 13: Introspection
