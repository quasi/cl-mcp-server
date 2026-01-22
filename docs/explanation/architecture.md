# Explanation: How CL-MCP-Server Works

**Purpose**: Understand the architecture, design decisions, and key concepts behind CL-MCP-Server.

## Overview

CL-MCP-Server is a bridge between Claude and a running Common Lisp environment. It allows Claude to:

- Send Lisp code for evaluation
- Receive results, output, and errors
- Maintain persistent state (like a REPL)
- Interact naturally through the Model Context Protocol

## Architecture Layers

```
┌─────────────────────────────────────────┐
│            Claude Code CLI              │
│  (or other MCP client)                  │
└─────────────────────────────────────────┘
                  │
                  │ JSON-RPC 2.0 over stdio
                  ▼
┌─────────────────────────────────────────┐
│         MCP Protocol Layer              │
│  • Message parsing/formatting           │
│  • Method routing                       │
│  • Error handling                       │
└─────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│         Session Management              │
│  • Persistent *package* context         │
│  • State tracking                       │
└─────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│         Code Evaluator                  │
│  • Read Lisp forms                      │
│  • Capture streams (stdout/stderr)      │
│  • Evaluate safely                      │
│  • Handle conditions                    │
│  • Format results                       │
└─────────────────────────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│      Common Lisp Runtime (SBCL)         │
└─────────────────────────────────────────┘
```

## Communication Flow

### 1. Initialization Handshake

When Claude Code starts the server:

```
Claude → {"method": "initialize", "params": {...}}
Server → {"result": {"protocolVersion": "2025-03-26", ...}}
Claude → {"method": "notifications/initialized"}
Server → (no response, notification)
```

**Purpose**: Establish protocol version and capabilities.

### 2. Tool Discovery

Claude asks what the server can do:

```
Claude → {"method": "tools/list"}
Server → {"result": {"tools": [{"name": "evaluate-lisp", ...}]}}
```

**Purpose**: Claude learns about the `evaluate-lisp` tool.

### 3. Code Evaluation

Claude sends code for evaluation:

```
Claude → {"method": "tools/call", "params": {"name": "evaluate-lisp", "arguments": {"code": "(+ 1 2)"}}}
Server → {"result": {"content": [{"type": "text", "text": "=> 3"}], "isError": false}}
```

**Purpose**: Execute Lisp code and return results.

## Key Concepts

### Persistent Session

Unlike a one-shot evaluator, the server maintains state:

```lisp
;; Evaluation 1
(defun square (x) (* x x))
=> SQUARE

;; Evaluation 2 (later)
(square 5)
=> 25  ; The function is still defined!
```

**How it works**:
- Single `*package*` instance shared across evaluations
- Symbol table persists
- Function definitions remain loaded

**Trade-off**: State accumulation (can't "undo" definitions without restart or reset).

### Stream Capture

The server redirects Lisp's standard streams during evaluation:

```lisp
;; User code
(progn
  (print "Hello")  ; Goes to *standard-output*
  (warn "Careful") ; Goes to *error-output*
  42)              ; Return value

;; Server response
[stdout]
"Hello"

[stderr]
WARNING: Careful

=> 42
```

**Why separate streams?**
- **stdout**: Intentional output (`print`, `format`)
- **stderr**: Warnings and error messages
- **return value**: The actual computed result

Claude can distinguish between what the code *produced* (output) and what it *returned* (value).

### Condition Handling

Common Lisp's condition system is rich. The server maps it to MCP error responses:

```lisp
;; User code
(/ 1 0)

;; Server catches the error
[ERROR] DIVISION-BY-ZERO
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).

[Backtrace]
0: (/ 1 0)
```

**Types of conditions**:
- **Warnings**: Captured and reported, but evaluation continues
- **Errors**: Caught, evaluation stops, error response returned
- **Serious conditions**: Caught to prevent server crash

**Critical invariant**: The server never crashes due to user code errors.

## Design Decisions

### Why stdio Transport?

**Choice**: Use stdin for requests, stdout for responses.

**Rationale**:
- Simple, universal (works everywhere)
- MCP standard transport
- No networking complexity
- Easy to launch as subprocess

**Trade-off**: Not suitable for browser or web-based clients.

### Why JSON-RPC 2.0?

**Choice**: MCP is built on JSON-RPC 2.0.

**Rationale**:
- Standard RPC protocol
- Simple request/response model
- Good tooling and documentation
- Language-agnostic

**Trade-off**: More verbose than custom protocol, but worth it for standardization.

### Why Persistent Session?

**Choice**: Maintain state across evaluations.

**Rationale**:
- Matches REPL experience (developers expect this)
- Enables incremental development
- Allows building up context

**Trade-off**: Can't easily "reset" without restarting server.

### Why Not Sandbox by Default?

**Current choice**: No aggressive sandboxing (can call any Lisp function).

**Rationale**:
- Trust model: Claude is trusted, user directs Claude
- Full Lisp expressiveness needed
- Sandboxing is complex and limiting

**Future consideration**: Could add opt-in restrictions (e.g., disallow file I/O, network access).

## Data Flow Example

Let's trace what happens when Claude sends `(+ 1 2)`:

### Step 1: Client Sends Request

```json
{
  "jsonrpc": "2.0",
  "id": 42,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2)"
    }
  }
}
```

**Transport**: Written to server's stdin as single-line JSON.

### Step 2: Server Parses Request

```lisp
;; Read line from stdin
;; Parse JSON → alist or hash-table
;; Extract method: "tools/call"
;; Extract params: (name: "evaluate-lisp", arguments: (code: "(+ 1 2)"))
```

### Step 3: Route to Handler

```lisp
;; Method routing
(case method
  ("tools/call" (handle-tools-call params))
  ...)
```

### Step 4: Extract Code

```lisp
;; From params
(let* ((tool-name (get-param params "name"))
       (arguments (get-param params "arguments"))
       (code (get-param arguments "code")))
  ;; code = "(+ 1 2)"
  ...)
```

### Step 5: Evaluate Safely

```lisp
(let ((stdout-stream (make-string-output-stream))
      (stderr-stream (make-string-output-stream))
      (*standard-output* stdout-stream)
      (*error-output* stderr-stream))
  (handler-case
      (handler-bind ((warning (lambda (c) ...)))
        (eval (read-from-string code)))
    (error (c) (make-error-response c))))
```

**Result**: `3` (no output, no errors)

### Step 6: Format Result

```lisp
;; Format as "=> 3"
(format nil "=> ~A" result)
```

### Step 7: Build Response

```json
{
  "jsonrpc": "2.0",
  "id": 42,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "=> 3"
      }
    ],
    "isError": false
  }
}
```

### Step 8: Send Response

**Transport**: Written to server's stdout as single-line JSON.

### Step 9: Client Receives

Claude Code receives the response and presents it to the user:

```
=> 3
```

## Error Handling Strategy

### Level 1: Evaluation Errors

```lisp
(handler-case (eval form)
  (error (c) (make-error-response c)))
```

**Catches**: Errors in user code (undefined functions, type errors, etc.)

### Level 2: Request Handling Errors

```lisp
(handler-case (handle-request request)
  (serious-condition (c) (make-json-rpc-error c)))
```

**Catches**: Errors in server code, malformed requests.

### Level 3: Server Loop

```lisp
(loop
  (handler-case (process-one-request)
    (serious-condition (c)
      (log-error c)
      ;; Continue loop - don't crash
      )))
```

**Catches**: Anything that slips through, ensuring server stays alive.

**Result**: Three layers of protection guarantee server stability.

## State Management

### What Persists?

| Item | Persists? | Scope |
|------|-----------|-------|
| Function definitions (`defun`) | ✓ | Global |
| Variable bindings (`defvar`, `defparameter`) | ✓ | Global |
| Class definitions (`defclass`) | ✓ | Global |
| Package context (`*package*`) | ✓ | Session |
| Let bindings (`let`, `let*`) | ✗ | Evaluation |
| Dynamic bindings (`let ((*var* ...))`) | ✗ | Evaluation |
| Loaded systems (`ql:quickload`) | ✓ | Global |

### Lifetime

```
Server Start
    ↓
Initialize Session (package = CL-USER)
    ↓
Evaluation 1 ───→ Define square
    ↓
Evaluation 2 ───→ Call square (available!)
    ↓
Evaluation 3 ───→ Redefine square (updates existing)
    ↓
...
    ↓
Server Shutdown (all state lost)
```

**No explicit "save" or "reset"** (for now - could be added).

## Performance Considerations

### Cold Start

**When**: Server process starts.

**Cost**:
- SBCL startup: ~100ms
- Quicklisp load: ~50ms
- Server init: ~10ms

**Total**: ~160ms

**Amortization**: One-time cost per conversation.

### Evaluation Latency

**Typical**: <10ms for simple expressions.

**Factors**:
- Parsing: ~1ms
- Evaluation: depends on code
- Formatting: ~1ms

**Optimization**: Keep server running (persistent process).

### Memory

**Base**: SBCL runtime ~50-100MB.

**Growth**: Depends on user definitions, data structures.

**Concern**: No automatic GC trigger (relies on Lisp's GC).

## Comparison to Other Approaches

### vs. Shelling Out to `sbcl`

| Approach | CL-MCP-Server | Shell `sbcl` |
|----------|---------------|--------------|
| State persistence | ✓ | ✗ |
| Startup cost | One-time | Per evaluation |
| Protocol | Structured MCP | Text output parsing |
| Error handling | Rich | Text scraping |

**Winner**: CL-MCP-Server for interactive use.

### vs. SLIME/SWANK

| Feature | CL-MCP-Server | SLIME |
|---------|---------------|-------|
| Transport | stdio | TCP socket |
| Client | Claude/MCP | Emacs |
| Protocol | JSON-RPC | S-expressions |
| Maturity | New | 20+ years |
| Features | Basic eval | Full IDE integration |

**Difference**: CL-MCP-Server is for AI agent interaction, SLIME is for human IDE.

## Security Implications

### Trust Model

**Assumption**: Claude is trusted, user directs Claude.

**Risk**: User can ask Claude to evaluate arbitrary Lisp code.

**Mitigation**: None currently (full Lisp access).

### Potential Threats

1. **File system access**: User could read/write files
2. **Network access**: User could make HTTP requests
3. **System commands**: User could run shell commands via `uiop:run-program`
4. **Resource exhaustion**: Infinite loops, memory allocation

### Future Hardening

- Opt-in sandboxing (restricted package)
- Time limits on evaluation
- Memory limits
- Disable dangerous operations

**Current state**: Proof-of-concept, not production-hardened.

## Extending the Server

### Adding New Tools

The architecture supports multiple MCP tools:

```lisp
;; Current
{"name": "evaluate-lisp", ...}

;; Future possibilities
{"name": "load-system", ...}
{"name": "describe-symbol", ...}
{"name": "search-documentation", ...}
```

Each tool would be a new entry in `tools/list` and a new handler in `tools/call`.

### Adding Features

Possible extensions:

- **Session reset**: Tool to clear all definitions
- **Package management**: Create, switch, delete packages
- **System loading**: Load Quicklisp systems
- **Documentation lookup**: Query symbol documentation
- **Code formatting**: Pretty-print code
- **Macro expansion**: Expand macros for inspection

## Next Steps

- **Try it**: [Quickstart Guide](../quickstart.md)
- **Learn by doing**: [Tutorial: First Session](../tutorials/01-first-session.md)
- **Deep dive**: [Reference: evaluate-lisp Tool](../reference/evaluate-lisp.md)
- **Troubleshoot**: [How-To: Common Issues](../how-to/troubleshooting.md)

---

**Now you understand how CL-MCP-Server works under the hood!**
