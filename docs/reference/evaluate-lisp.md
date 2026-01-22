# Reference: evaluate-lisp Tool

**Official specification for the `evaluate-lisp` MCP tool.**

## Tool Definition

```json
{
  "name": "evaluate-lisp",
  "description": "Evaluate Common Lisp code in a persistent REPL session. Definitions and variables persist across calls.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp expression(s) to evaluate. Can be a single form or multiple forms."
      },
      "package": {
        "type": "string",
        "description": "Package context for evaluation. Defaults to CL-USER or the last-used package."
      }
    }
  }
}
```

## Input Parameters

### code (required)

**Type**: `string`

**Description**: Common Lisp expression(s) to evaluate.

**Format**:
- Single form: `"(+ 1 2)"`
- Multiple forms: `"(defun square (x) (* x x)) (square 5)"`
- Whitespace/newlines: Allowed and ignored

**Parsing**:
- Code is read using the Lisp reader (`read-from-string`)
- Multiple forms are read and evaluated in sequence
- Only the result of the **last form** is returned

**Examples**:

```json
{"code": "(+ 1 2 3)"}
{"code": "(mapcar #'1+ '(1 2 3))"}
{"code": "(defun foo (x) (* x 2))\n(foo 21)"}
```

### package (optional)

**Type**: `string`

**Description**: Package context for evaluation.

**Default**: Current session package (initially `CL-USER`).

**Behavior**:
- If provided, `*package*` is bound to that package during evaluation
- Package name is case-insensitive (`"cl-user"` = `"CL-USER"`)
- If package doesn't exist, evaluation fails with `PACKAGE-ERROR`

**Examples**:

```json
{"code": "(in-package :my-package)", "package": "CL-USER"}
{"code": "(defun foo () ...)", "package": "MY-PACKAGE"}
```

**Note**: Package switching via `in-package` in the code itself also works and updates session state.

## Output Format

### Success Response

```json
{
  "content": [
    {
      "type": "text",
      "text": "{formatted result}"
    }
  ],
  "isError": false
}
```

### Text Format

The `text` field contains:

```
[stdout]
{captured standard output}

[stderr]
{captured error output}

[warnings]
{captured warnings}

=> {return value 1}
=> {return value 2}
...
```

**Section rules**:
- Sections are **omitted if empty**
- `[stdout]` appears only if `*standard-output*` received output
- `[stderr]` appears only if `*error-output*` received output
- `[warnings]` appears only if warnings were signaled
- `=> value` lines appear for each return value (multiple values)

### Examples

#### Simple Value

**Input**: `{"code": "(+ 1 2)"}`

**Output**:
```
=> 3
```

#### With stdout

**Input**: `{"code": "(progn (print 'hello) 42)"}`

**Output**:
```
[stdout]
HELLO

=> 42
```

#### Multiple Values

**Input**: `{"code": "(floor 17 5)"}`

**Output**:
```
=> 3
=> 2
```

#### With Warnings

**Input**: `{"code": "(defun foo () (let ((x 10))))"}`

**Output**:
```
[warnings]
STYLE-WARNING: The variable X is defined but never used.

=> FOO
```

## Error Response

When evaluation signals an unhandled error:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{message}\n\n[Backtrace]\n{frames}"
    }
  ],
  "isError": true
}
```

**Key**: `isError: true` distinguishes errors from success.

### Error Text Format

```
[ERROR] {CONDITION-TYPE-NAME}
{formatted condition message}

[Backtrace]
{frame 0}
{frame 1}
...
```

### Examples

#### Division by Zero

**Input**: `{"code": "(/ 1 0)"}`

**Output**:
```
[ERROR] DIVISION-BY-ZERO
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).

[Backtrace]
0: (/ 1 0)
```

#### Undefined Function

**Input**: `{"code": "(foo 42)"}`

**Output**:
```
[ERROR] UNDEFINED-FUNCTION
The function COMMON-LISP-USER::FOO is undefined.

[Backtrace]
0: (FOO 42)
```

#### Type Error

**Input**: `{"code": "(car 42)"}`

**Output**:
```
[ERROR] TYPE-ERROR
The value 42 is not of type LIST.

[Backtrace]
0: (CAR 42)
```

## Value Printing

Return values are printed using `prin1-to-string` with these settings:

| Variable | Value | Purpose |
|----------|-------|---------|
| `*print-length*` | 100 | Truncate lists longer than 100 elements |
| `*print-level*` | 10 | Truncate nesting deeper than 10 levels |
| `*print-circle*` | `t` | Handle circular structures safely |
| `*print-pretty*` | `t` | Use pretty-printer for readability |
| `*print-readably*` | `nil` | Allow unreadable objects |

### Truncation Example

**Input**: `{"code": "(make-list 200)"}`

**Output**:
```
=> (NIL NIL NIL ... NIL)  ; Truncated at 100 elements
```

### Circular Structure Example

**Input**:
```json
{
  "code": "(let ((x (list 1 2 3))) (setf (cdddr x) x) x)"
}
```

**Output**:
```
=> #1=(1 2 3 . #1#)  ; Circular reference notation
```

## Stream Capture

During evaluation, these streams are redirected:

| Stream | Target | Appears As |
|--------|--------|------------|
| `*standard-output*` | String stream | `[stdout]` section |
| `*error-output*` | String stream | `[stderr]` section |
| `*trace-output*` | String stream | `[stderr]` section |
| `*debug-io*` | Null stream | (suppressed) |
| `*query-io*` | Null stream | (suppressed) |

**Interactive I/O is suppressed** because there's no way to interact with the user through MCP.

### Example

**Input**:
```json
{
  "code": "(progn (format t \"Output~%\") (format *error-output* \"Error~%\") 42)"
}
```

**Output**:
```
[stdout]
Output

[stderr]
Error

=> 42
```

## Condition Handling

### Warning Handling

Warnings are **captured but don't stop evaluation**:

```lisp
(handler-bind ((warning (lambda (c)
                          (record-warning c)
                          (muffle-warning c))))
  (eval form))
```

**Result**: Warnings appear in `[warnings]` section, evaluation continues.

### Error Handling

Errors are **caught and reported**:

```lisp
(handler-case (eval form)
  (error (c)
    (make-error-response c)))
```

**Result**: Error response with `isError: true`.

### Condition Types

| Condition Class | Handling |
|----------------|----------|
| `warning` | Record and muffle |
| `style-warning` | Record and muffle |
| `error` | Catch, format, return error response |
| `serious-condition` | Catch, format, return error response |
| `storage-condition` | Catch, format, return error response |

## Multiple Forms

When the `code` parameter contains multiple forms, they are evaluated in sequence:

**Input**:
```json
{
  "code": "(setf x 10) (setf y 20) (+ x y)"
}
```

**Evaluation**:
1. `(setf x 10)` → 10 (returned, not shown)
2. `(setf y 20)` → 20 (returned, not shown)
3. `(+ x y)` → 30 (returned, shown)

**Output**:
```
=> 30
```

**Only the last form's result is returned.**

**Side effects** (like `setf`) from all forms persist in the session.

## State Persistence

Definitions and bindings made in one evaluation **persist** in subsequent evaluations:

### Example Session

**Evaluation 1**:
```json
{"code": "(defun square (x) (* x x))"}
```

**Response 1**:
```
=> SQUARE
```

**Evaluation 2** (later):
```json
{"code": "(square 7)"}
```

**Response 2**:
```
=> 49
```

**Key**: The `square` function is still defined.

### What Persists

| Item | Persists? |
|------|-----------|
| Function definitions (`defun`, `defmethod`) | ✓ |
| Variable bindings (`defvar`, `defparameter`, `setf`) | ✓ |
| Class definitions (`defclass`, `defstruct`) | ✓ |
| Package context (`*package*`) | ✓ |
| Loaded systems (`ql:quickload`) | ✓ |
| Local bindings (`let`, `flet`) | ✗ |
| Dynamic bindings | ✗ |

## Backtrace Format

When an error occurs, a backtrace is included:

```
[Backtrace]
0: (FUNCTION-NAME ARG1 ARG2 ...)
1: (CALLER-NAME ...)
...
```

**Format**:
- Frame number: 0-indexed
- Function name: Uppercase
- Arguments: Printed readably (subject to `*print-length*`)

**Filtering**:
- Internal evaluator frames are skipped
- Up to 20 user-relevant frames shown

## Protocol Context

This tool is invoked via the MCP `tools/call` method:

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2)"
    }
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{"type": "text", "text": "=> 3"}],
    "isError": false
  }
}
```

## Limitations

### No Interactive I/O

**Cannot use**:
- `read` from `*query-io*` or `*debug-io*`
- `y-or-n-p`, `yes-or-no-p`
- Debugger interactions

**Reason**: MCP is request-response, not interactive.

### No Asynchronous Evaluation

**Cannot**:
- Start background threads that send results later
- Use callbacks or promises

**Reason**: Response must be synchronous.

### Resource Limits

**Current state**: No enforced limits on:
- Evaluation time (infinite loops will hang)
- Memory usage (can exhaust memory)
- Output size (large output may be slow)

**Future**: Could add opt-in limits.

## Best Practices

### For Users (via Claude)

1. **Test incrementally**: Define functions one at a time
2. **Check for errors**: Read error messages carefully
3. **Use descriptive names**: Makes code self-documenting
4. **Avoid infinite loops**: Start simple, add complexity

### For Client Implementations

1. **Set reasonable timeouts**: Don't wait forever for evaluation
2. **Handle large output**: Be prepared for multi-line responses
3. **Parse `isError` flag**: Distinguish success from failure
4. **Preserve formatting**: Show `[stdout]`, `[stderr]`, `[warnings]` sections clearly

## Related Documentation

- **Quickstart**: [Get Running in 5 Minutes](../quickstart.md)
- **Tutorial**: [Your First REPL Session](../tutorials/01-first-session.md)
- **Explanation**: [How It Works](../explanation/architecture.md)
- **Canon**: For formal specification, see `canon/features/code-evaluator/contracts/evaluate-lisp-tool.md`

---

**This is the complete reference for the `evaluate-lisp` tool.**
