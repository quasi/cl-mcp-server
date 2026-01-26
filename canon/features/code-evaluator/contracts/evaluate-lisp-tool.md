---
type: contract
name: evaluate-lisp-tool
version: 0.1.0
---

# Evaluate-Lisp Tool Contract

The primary tool for executing Common Lisp code.

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
      },
      "capture-time": {
        "type": "boolean",
        "description": "If true, include timing information (real time, run time, GC time, bytes consed) in the result."
      }
    }
  }
}
```

## Input Processing

### Code Parsing

The `code` parameter is read using the Lisp reader:
1. If multiple forms are present, each is evaluated in sequence
2. Only the result of the last form is returned
3. Reader errors are caught and reported

### Package Context

The `package` parameter:
1. If provided, `*package*` is bound to that package during evaluation
2. If omitted, uses the session's current package (defaults to CL-USER)
3. Invalid package names signal an error

### Timing Capture (Phase B)

The `capture-time` parameter:
1. If true, captures execution timing information
2. If false or omitted, no timing is captured
3. Timing includes: real time, run time, GC time, bytes consed

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

### Result Formatting

The text content follows this structure:

```
[stdout]
{any output to *standard-output*}

[stderr]
{any output to *error-output*}

[warnings]
STYLE-WARNING: {warning message}

=> {primary return value, printed readably}
=> {second value}
=> {third value}
...
```

Sections are omitted if empty:
- No `[stdout]` section if nothing was printed
- No `[stderr]` section if no error output
- No `[warnings]` section if no warnings signaled
- Multiple `=>` lines only for multiple values
- Timing section only if `capture-time` was true

### Timing Output (when capture-time is true)

```
; Timing: {N}ms real, {N}ms run, {N}ms GC, {N} bytes consed
```

### Value Printing

Return values are printed using `prin1-to-string` with:
- `*print-length*` = 100 (truncate long lists)
- `*print-level*` = 10 (truncate deep nesting)
- `*print-circle*` = t (handle circular structures)
- `*print-pretty*` = t (readable formatting)

## Error Response

When evaluation signals an unhandled error:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{condition message}\n\n[Backtrace]\n{formatted backtrace}"
    }
  ],
  "isError": true
}
```

### Backtrace Formatting

Backtraces are truncated to the most relevant frames:
1. Skip internal evaluator frames
2. Show up to 20 user-relevant frames
3. Format: `N: (FUNCTION-NAME ARG1 ARG2 ...)`

## Captured Streams

During evaluation, the following streams are redirected:

| Stream | Captured To | Section |
|--------|-------------|---------|
| `*standard-output*` | String | `[stdout]` |
| `*error-output*` | String | `[stderr]` |
| `*trace-output*` | String | `[stderr]` |
| `*debug-io*` | Null | (suppressed) |
| `*query-io*` | Null | (suppressed) |

Interactive I/O (`*debug-io*`, `*query-io*`) is suppressed because there's no way to interact with the user through MCP.

## Condition Handling

### Warnings

Warnings are captured but don't stop evaluation:

```lisp
(handler-bind ((warning (lambda (c)
                          (record-warning c)
                          (muffle-warning c))))
  (eval form))
```

### Errors

Errors are caught and reported:

```lisp
(handler-case (eval form)
  (error (c)
    (make-error-response c)))
```

### Specific Condition Types

| Condition | Handling |
|-----------|----------|
| `warning` | Record and muffle |
| `style-warning` | Record and muffle |
| `error` | Catch, format, return as error |
| `serious-condition` | Catch, format, return as error |
| `storage-condition` | Catch, format, return as error |
