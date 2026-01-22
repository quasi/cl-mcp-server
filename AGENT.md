# AGENT.md

**Contributing Agent Instructions for CL-MCP-Server**

## Project Context

**Name**: cl-mcp-server
**Language**: Common Lisp
**Implementation**: SBCL (Steel Bank Common Lisp)
**Build System**: ASDF
**Protocol**: Model Context Protocol (MCP) 2025-03-26
**Canon Version**: 0.1.0

## What This Project Does

This is an MCP server that enables Claude to send Common Lisp code for evaluation in a running Lisp instance. It provides:

- JSON-RPC 2.0 communication over stdio
- Persistent REPL session with state management
- Safe code evaluation with condition handling
- Structured output (values, stdout, stderr, warnings)

## Build Commands

### Setup

```bash
# Ensure Quicklisp is installed
sbcl --load ~/quicklisp/setup.lisp

# Load the system
sbcl --load cl-mcp-server.asd --eval "(ql:quickload :cl-mcp-server)"
```

### Running the Server

```bash
# Start the MCP server
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)" \
     --eval "(cl-mcp-server:start)"

# Or use the launcher script
./run-server.lisp
```

### Testing

```bash
# Load and run all tests (when test system exists)
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(asdf:test-system :cl-mcp-server)"

# Run specific test suite
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:evaluator-suite)"
```

### Development

```bash
# Load in REPL for interactive development
sbcl
* (ql:quickload :cl-mcp-server)
* (in-package :cl-mcp-server)
```

## Interactive Development with the Lisp MCP Server

**When the `lisp` MCP tools are available, prefer interactive incremental development over edit-compile-run cycles.**

### Detecting Availability

Check if these tools are available in your session:
- `mcp__lisp__evaluate-lisp` - Evaluate Common Lisp code
- `mcp__lisp__load-system` - Load ASDF systems
- `mcp__lisp__list-definitions` - List current definitions
- `mcp__lisp__reset-session` - Reset to fresh state

### Workflow: Incremental Development

**Instead of**: Edit file → Save → Reload system → Test → Repeat

**Do this**: Evaluate code directly → Test immediately → Iterate → Write to file when stable

```
1. Load the system once:     (ql:quickload :cl-mcp-server)
2. Redefine functions:       (defun foo () ...)  ; immediate effect
3. Test interactively:       (foo)               ; instant feedback
4. Iterate until correct:    (defun foo () ...)  ; redefine as needed
5. Write to file:            Only after code is working
```

### When to Use Interactive Evaluation

| Situation | Approach |
|-----------|----------|
| Exploring/understanding code | Evaluate expressions directly |
| Developing new function | Define in REPL, iterate, then write to file |
| Debugging | Evaluate subexpressions, inspect values |
| Testing changes | Redefine function, call it immediately |
| Running tests | `(fiveam:run! 'suite-name)` |
| Quick experiments | Evaluate without touching files |

### When to Edit Files Directly

| Situation | Approach |
|-----------|----------|
| Adding new files | Write tool (must update .asd too) |
| Package/export changes | Edit file, then reload system |
| Macro definitions | Edit file (macro callers need recompilation) |
| Final implementation | Write stable code to file |
| Structural changes | Edit files, reload affected systems |

### Example Session

```lisp
;; 1. Load system
(ql:quickload :cl-mcp-server)

;; 2. Develop a helper function interactively
(defun format-result (value)
  (format nil "=> ~S" value))

;; 3. Test it
(format-result 42)
;; => "=> 42"

;; 4. Refine it
(defun format-result (value)
  (format nil "=> ~A" value))  ; Changed ~S to ~A

;; 5. Test again
(format-result "hello")
;; => "=> hello"

;; 6. Now write to file (only after it works)
```

### Benefits

- **Faster feedback**: No startup overhead, instant results
- **State preservation**: Definitions persist across evaluations
- **Exploratory**: Inspect values, test edge cases interactively
- **Reduced context switching**: Stay in the development flow

### Caveats

- **Macros**: Redefining a macro doesn't update existing callers; reload dependents
- **Package changes**: Export list changes require file edit and reload
- **Session state**: Use `reset-session` if state becomes inconsistent
- **File sync**: Remember to write working code to files before ending session

## Code Conventions

### Package Structure

- Use `defpackage` with explicit `:export` lists
- Package names follow lowercase hyphenated style: `cl-mcp-server`, `cl-mcp-server.evaluator`
- Internal packages use `/internal` suffix when needed

### Naming Conventions

| Type | Convention | Example |
|------|------------|---------|
| Special variables | `*earmuffs*` | `*session-state*` |
| Constants | `+plus-signs+` | `+protocol-version+` |
| Predicates | `-p` suffix | `error-response-p` |
| Constructors | `make-` prefix | `make-error-response` |
| Functions | hyphenated lowercase | `evaluate-code` |
| Classes/Structs | hyphenated lowercase | `evaluation-result` |

### File Organization

Each file should begin with an ABOUTME comment:

```lisp
;;;; ABOUTME: Brief description of what this file provides
;;;;
;;;; Additional context or design notes if needed.
```

### Error Handling

- **NEVER** let evaluation errors crash the server (see INV-002)
- Use `handler-case` for expected errors
- Use `handler-bind` for warnings (record and muffle)
- Always return structured responses even for errors

Example pattern:

```lisp
(handler-case
    (handler-bind ((warning (lambda (c)
                              (record-warning c)
                              (muffle-warning c))))
      (eval form))
  (error (c)
    (make-error-response c)))
```

### Stream Handling

During evaluation, redirect streams:

```lisp
(let ((*standard-output* stdout-string-stream)
      (*error-output* stderr-string-stream)
      (*trace-output* stderr-string-stream)
      (*debug-io* (make-broadcast-stream))
      (*query-io* (make-broadcast-stream)))
  ;; evaluation here
  )
```

## Architecture Rules

### RULE-001: Request-Response Guarantee

Every valid JSON-RPC request MUST receive exactly one response.

**Correct**:
```lisp
(defun handle-request (request)
  (handler-case
      (process-request request)
    (error (c)
      ;; Still return a response, even on error
      (make-error-response (request-id request) c))))
```

**Violation**:
```lisp
(defun handle-request (request)
  ;; This can fail to respond if process-request signals an error
  (process-request request))
```

### RULE-002: Server Stability

The server process MUST NOT terminate due to evaluation errors.

**Correct**:
```lisp
(defun server-loop ()
  (loop
    (handler-case
        (handle-one-request)
      (serious-condition (c)
        ;; Log but continue
        (log-error c)))))
```

**Violation**:
```lisp
(defun server-loop ()
  (loop
    ;; Unhandled errors will crash the server
    (handle-one-request)))
```

### RULE-003: Session State Persistence

Definitions made in one evaluation MUST be available in subsequent evaluations.

**Correct**:
```lisp
;; Evaluate in a persistent package environment
(defvar *session-package* (find-package :cl-user))

(defun evaluate (code)
  (let ((*package* *session-package*))
    (eval (read-from-string code))))
```

**Violation**:
```lisp
(defun evaluate (code)
  ;; Creating new package each time loses state
  (let ((*package* (make-package (gensym "TEMP-"))))
    (eval (read-from-string code))))
```

### RULE-004: Output Stream Separation

Return values, stdout, stderr, and warnings MUST be distinguishable.

**Correct**:
```lisp
{
  "content": [{"type": "text", "text": "[stdout]\nHello\n\n=> 42"}],
  "isError": false
}
```

**Violation**:
```lisp
{
  "content": [{"type": "text", "text": "Hello42"}],
  "isError": false
}
```

### RULE-005: Condition Type Preservation

When errors occur, preserve the condition type, not just the message.

**Correct**:
```lisp
(format nil "[ERROR] ~A~%~A" (type-of condition) condition)
;; => "[ERROR] DIVISION-BY-ZERO\narithmetic error ..."
```

**Violation**:
```lisp
(format nil "Error: ~A" condition)
;; => "Error: arithmetic error ..." (type lost)
```

## File Locations

| Type | Location | Purpose |
|------|----------|---------|
| System definition | `cl-mcp-server.asd` | ASDF system |
| Source code | `*.lisp` (root) | Implementation |
| Tests | `tests/*.lisp` | Test suites (future) |
| Canon specs | `canon/` | Formal specifications |
| Documentation | `docs/` | Generated docs (future) |
| Launcher | `run-server.lisp` | Executable entry point |

## Protocol Conformance

### MCP Version

This server implements MCP protocol version `2025-03-26`.

### Required Methods

| Method | Purpose | Handler |
|--------|---------|---------|
| `initialize` | Handshake | Returns server info |
| `notifications/initialized` | Client ready signal | No-op |
| `tools/list` | List available tools | Returns `evaluate-lisp` |
| `tools/call` | Execute tool | Evaluates code |

### JSON-RPC 2.0 Requirements

- All messages are valid JSON objects
- Requests include: `jsonrpc`, `method`, `id` (for requests), `params`
- Responses include: `jsonrpc`, `id`, `result` OR `error`
- Notifications omit `id`

## Key Invariants

From `canon/core/foundation/invariants.md`:

1. **INV-001**: Every valid JSON-RPC request receives exactly one response
2. **INV-002**: Server never terminates due to evaluation errors
3. **INV-003**: Session state persists across evaluations
4. **INV-004**: Output streams (stdout/stderr/values) are distinguishable
5. **INV-005**: All messages conform to JSON-RPC 2.0
6. **INV-006**: Condition types are preserved in error reports

**Verify these invariants when implementing or modifying code.**

## Domain Model

### Core Entities

```
MCP Server
  ├─ maintains → Session (1:1)
  ├─ uses → Transport (stdio) (1:1)
  └─ exposes → Tools (1:N)

Session
  ├─ has current → Package (1:1)
  └─ contains → Evaluations (1:N)

Evaluation
  ├─ produces → Result (1:1)
  ├─ captures → Output (stdout/stderr) (1:1)
  └─ may signal → Conditions (1:0..N)
```

See `canon/core/foundation/ontology.md` for full diagrams.

## Navigation

For detailed specifications and implementation guidance:

**Entry Point**: `canon/INDEX.md`

**Quick Links**:
- Understand the domain: `canon/core/foundation/vocabulary.md`
- Implement MCP protocol: `canon/features/mcp-protocol/`
- Implement code evaluator: `canon/features/code-evaluator/`
- Handle errors: `canon/features/error-handling/`
- Manage sessions: `canon/features/session-management/`

## Testing Strategy

### Test Framework

Use FiveAM (planned):

```lisp
(defpackage :cl-mcp-server-tests
  (:use :cl :fiveam)
  (:export #:run-tests))

(in-package :cl-mcp-server-tests)

(def-suite evaluator-suite
  :description "Code evaluation tests")

(in-suite evaluator-suite)

(test basic-arithmetic
  (is (equal "=> 6" (evaluate-and-format "(+ 1 2 3)"))))
```

### Test Coverage

- **Unit tests**: Individual functions (parse, format, evaluate)
- **Integration tests**: Full request-response cycles
- **Protocol tests**: MCP handshake and tool calls
- **Error tests**: Various error conditions and recovery

### Scenario-Based Testing

Use scenarios from `canon/features/*/scenarios/` as test cases:

```lisp
;; From canon/features/code-evaluator/scenarios/basic-evaluation.md
(test arithmetic-expression
  (let ((request (make-request :code "(+ 1 2 3)")))
    (let ((response (handle-tools-call request)))
      (is (equal "=> 6" (extract-result-text response)))
      (is (not (response-is-error-p response))))))
```

## Common Pitfalls

### Stream Capture

**Problem**: Output doesn't appear in response.

**Solution**: Ensure `*standard-output*` and `*error-output*` are bound to string streams during evaluation.

### Package Context

**Problem**: Symbols undefined in subsequent evaluations.

**Solution**: Maintain persistent `*package*` across evaluations.

### Error Handling

**Problem**: Server crashes on evaluation errors.

**Solution**: Wrap evaluation in `handler-case` at the server loop level.

### Circular Structures

**Problem**: Printing circular lists hangs.

**Solution**: Bind `*print-circle*` to `t` before printing.

## Issue Tracking

This project uses **bd (beads)** for issue tracking.

**Quick reference**:
- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd update <id> --status in_progress` - Start work
- `bd close <id>` - Complete work
- `bd sync --flush-only` - Export to JSONL

For full workflow: `bd prime`

---

**Last Updated**: 2026-01-22
**Canon Version**: 0.1.0
