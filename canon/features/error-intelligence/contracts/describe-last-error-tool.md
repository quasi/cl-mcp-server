---
type: contract
name: describe-last-error-tool
version: 0.1.0
---

# Describe-Last-Error Tool Contract

Get detailed information about the most recent error including type, message,
available restarts, and backtrace information.

## Tool Definition

```json
{
  "name": "describe-last-error",
  "description": "Get detailed information about the most recent error. Returns the error type, message, available restarts, and backtrace from the last failed evaluation. Useful for diagnosing why code failed.",
  "inputSchema": {
    "type": "object",
    "properties": {}
  }
}
```

## State Management

### Last Error Tracking

The server maintains a reference to the most recent error condition:

1. **Set on error**: When any tool invocation produces an error
2. **Preserved**: Remains available until next successful operation
3. **Cleared**: Reset on successful evaluation or explicit reset
4. **Accessible**: Available via this tool at any time

### Error Context

The captured error context includes:

| Field | Description |
|-------|-------------|
| Type | Condition class name |
| Message | Formatted error message |
| Restarts | Available recovery options |
| Backtrace | Stack frames (abbreviated) |
| Timestamp | When error occurred |

## Output Format

### Success Response

```
Error: DIVISION-BY-ZERO
  arithmetic error DIVISION-BY-ZERO signalled
  Operation was (/ 1 0).

Available Restarts:
  1. ABORT - Return to top level
  2. RETRY - Retry SLIME REPL evaluation request

Backtrace (top 5 frames):
  0: (/ 1 0)
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (/ 1 0) #<NULL-LEXENV>)
  2: (EVAL (/ 1 0))
  3: (HANDLE-TOOL-CALL "evaluate-lisp" ...)
  4: (PROCESS-REQUEST ...)

For full backtrace, use get-backtrace tool.
```

### No Error Available

```
No error information available.
(No error has occurred since the last successful evaluation)
```

## Error Information Extraction

### Condition Type

The error type is extracted via:

```lisp
(type-of condition)  ; → DIVISION-BY-ZERO
```

Condition types follow the standard hierarchy:
- `ERROR` - All errors
- `ARITHMETIC-ERROR` - Math problems
- `TYPE-ERROR` - Type mismatches
- `UNBOUND-VARIABLE` - Variable not defined
- etc.

### Error Message

Formatted using:

```lisp
(princ-to-string condition)
;; → "arithmetic error DIVISION-BY-ZERO signalled"
```

Additional details from condition slots if available.

### Available Restarts

Retrieved via:

```lisp
(compute-restarts condition)
```

Each restart includes:
- Name (e.g., `ABORT`, `RETRY`, `USE-VALUE`)
- Description (interactive prompt text)

### Backtrace Preview

Top 5 frames from the stack trace, formatted for readability.
Full backtrace available via `get-backtrace` tool.

## Examples

### Division by Zero

```
Error: DIVISION-BY-ZERO
  arithmetic error DIVISION-BY-ZERO signalled
  Operation was (/ 1 0).

Available Restarts:
  1. ABORT - Return to top level

Backtrace (top 5 frames):
  0: (/ 1 0)
  1: (EVAL (/ 1 0))
  2: (HANDLE-EVALUATE-LISP ...)
```

### Undefined Function

```
Error: UNDEFINED-FUNCTION
  The function NONEXISTENT-FUNC is undefined.

Available Restarts:
  1. ABORT - Return to top level
  2. RETRY - Retry calling NONEXISTENT-FUNC
  3. USE-VALUE - Call specified function instead
  4. RETURN-VALUE - Return specified values

Backtrace (top 5 frames):
  0: (SB-KERNEL::UNDEFINED-FUN-ERROR NONEXISTENT-FUNC)
  1: (NONEXISTENT-FUNC 42)
  2: (EVAL (NONEXISTENT-FUNC 42))
```

### Type Error

```
Error: TYPE-ERROR
  The value "string" is not of type NUMBER.

Available Restarts:
  1. ABORT - Return to top level

Backtrace (top 5 frames):
  0: (+ 1 "string")
  1: (EVAL (+ 1 "string"))
```

### No Error

```
No error information available.
(No error has occurred since the last successful evaluation)
```

## Implementation Notes

### Error Capture Strategy

```lisp
;; Global error state
(defvar *last-error-condition* nil)
(defvar *last-error-time* nil)
(defvar *last-error-backtrace* nil)

;; Capture in error handler
(handler-bind
  ((error
    (lambda (c)
      (setf *last-error-condition* c
            *last-error-time* (get-universal-time)
            *last-error-backtrace* (sb-debug:list-backtrace)))))
  (eval code))
```

### Restart Collection

Use `compute-restarts` at the error point to capture available restarts.
Restarts are dynamic and context-dependent.

### Backtrace Abbreviation

Show top 5 frames for overview. Full backtrace can be hundreds of frames
deep (including SBCL internals).

## Verification Strategy

Tests should verify:

1. **Error captured**: After error, tool returns error info
2. **Type correct**: Specific condition type, not generic "ERROR"
3. **Message present**: Non-empty, meaningful message
4. **Restarts available**: At least ABORT restart present
5. **Backtrace preview**: Top frames shown
6. **No error case**: Appropriate message when no error
7. **Error persistence**: Error info available across tool calls
8. **Error cleared**: Successful evaluation clears last error
