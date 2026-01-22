---
type: contract
name: condition-report
version: 0.1.0
---

# Condition Report Contract

Defines how Lisp conditions are captured and reported.

## Condition Response Structure

When an unhandled error occurs:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{message}\n\n[Backtrace]\n{formatted-backtrace}"
    }
  ],
  "isError": true
}
```

## Condition Information Captured

### Required Fields

| Field | Description | Example |
|-------|-------------|---------|
| Type | Condition class name | `UNDEFINED-FUNCTION` |
| Message | Human-readable description | `The function FOO is undefined.` |

### Optional Fields

| Field | Description | When Included |
|-------|-------------|---------------|
| Backtrace | Stack trace | When available |
| Slots | Condition-specific data | For typed conditions |

## Common Condition Types

### Evaluation Errors

| Condition | When Signaled | Example Message |
|-----------|---------------|-----------------|
| `UNDEFINED-FUNCTION` | Unknown function called | `The function FOO is undefined.` |
| `UNBOUND-VARIABLE` | Unknown variable referenced | `The variable X is unbound.` |
| `TYPE-ERROR` | Type mismatch | `The value "hello" is not of type NUMBER.` |
| `DIVISION-BY-ZERO` | Division by zero | `Arithmetic error: division by zero.` |
| `PACKAGE-ERROR` | Package issues | `Package "NONEXISTENT" not found.` |

### Reader Errors

| Condition | When Signaled | Example Message |
|-----------|---------------|-----------------|
| `READER-ERROR` | Malformed syntax | `Unmatched close parenthesis.` |
| `END-OF-FILE` | Incomplete form | `Unexpected end of file.` |

### System Conditions

| Condition | When Signaled | Example Message |
|-----------|---------------|-----------------|
| `STORAGE-CONDITION` | Memory exhausted | `Heap exhausted.` |
| `SERIOUS-CONDITION` | Unrecoverable error | Various |

## Backtrace Format

```
[Backtrace]
0: (FOO 1 2 3)
1: (BAR X)
2: (USER-FUNCTION)
3: (EVAL (USER-FUNCTION))
...
```

### Backtrace Rules

1. Show at most 20 frames
2. Skip internal implementation frames (evaluator, handler machinery)
3. Start from the point of error
4. Truncate long argument lists with `...`
5. Use `#<...>` for unprintable objects

## Warning Report Format

Warnings don't set `isError` to true:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[warnings]\nSTYLE-WARNING: Using deprecated function OLD-FN.\nWARNING: Undefined variable assumed special.\n\n=> {result}"
    }
  ],
  "isError": false
}
```

### Warning Types

| Type | Severity | Example |
|------|----------|---------|
| `STYLE-WARNING` | Low | Unused variable |
| `WARNING` | Medium | Deprecated usage |
| `COMPILER-WARNING` | Medium | Optimization notes |

## Error with Output

When code produces output before erroring:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[stdout]\nProcessing item 1...\nProcessing item 2...\n\n[ERROR] DIVISION-BY-ZERO\nArithmetic error: division by zero.\n\n[Backtrace]\n..."
    }
  ],
  "isError": true
}
```

## Implementation Notes

### Using handler-bind

```lisp
(handler-bind
    ((warning (lambda (c)
                (push (format-warning c) *warnings*)
                (muffle-warning c)))
     (error (lambda (c)
              (return-from evaluate
                (make-error-result c (capture-backtrace))))))
  (eval form))
```

### Backtrace Capture

For SBCL:
```lisp
(sb-debug:list-backtrace)
;; or
(sb-debug:print-backtrace :stream s :count 20)
```

For portability, consider `trivial-backtrace`:
```lisp
(trivial-backtrace:print-backtrace condition :stream s)
```
