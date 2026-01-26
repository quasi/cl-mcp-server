# Error Intelligence Vocabulary

Terms specific to error capture, backtraces, and debugging.

---

## Error Context

An **Error Context** is the complete information captured when an error
occurs:

- Error type (condition class)
- Error message
- Backtrace (call stack)
- Available restarts
- Source location (if available)

---

## Backtrace

A **Backtrace** (or stack trace) is the sequence of function calls leading
to an error:

```
0: (/ 1 0)
1: (BAD-DIVIDE)
2: (MY-FUNCTION 42)
3: (EVAL ...)
```

Each frame shows:
- Frame number (0 = innermost)
- Function name
- Arguments (if available)

---

## Stack Frame

A **Stack Frame** represents one function call in the backtrace:

| Field | Description |
|-------|-------------|
| Number | Frame index (0 = error point) |
| Function | Function name or lambda |
| Arguments | Argument values if available |
| Source | Source location if known |

---

## Restart

A **Restart** is a recovery option available at an error point:

Common Lisp's condition system provides restarts like:
- `ABORT` - Return to top level
- `RETRY` - Try the operation again
- `USE-VALUE` - Provide an alternate value
- `STORE-VALUE` - Set and use a value
- `CONTINUE` - Proceed ignoring the error

---

## Condition

A **Condition** is Common Lisp's generalization of errors, warnings, and
other exceptional situations. All errors are conditions.

Hierarchy:
```
CONDITION
  └─ SERIOUS-CONDITION
      └─ ERROR
          ├─ ARITHMETIC-ERROR
          │   ├─ DIVISION-BY-ZERO
          │   └─ FLOATING-POINT-OVERFLOW
          ├─ TYPE-ERROR
          ├─ UNBOUND-VARIABLE
          └─ ...
```

---

## sb-debug

**sb-debug** is SBCL's debugging module providing:

- `backtrace` - Get stack frames
- `print-backtrace` - Format backtrace to stream
- `list-locals` - Show local variables in frame
- Frame navigation utilities

Loaded automatically when errors occur.

---

## Debugger State

The **Debugger State** is SBCL's internal representation of an error
condition, including:
- The signaled condition
- The continuation (where to resume)
- Available restarts
- Stack frame information

This state is captured and stored for inspection.

---

## Last Error

The **Last Error** is a server-maintained reference to the most recent
error condition and its context. Used by `describe-last-error` and
`get-backtrace` tools.

Cleared on successful evaluation or manual reset.

---

## Frame Locals

**Frame Locals** are the local variables visible in a stack frame:

```
Frame 2: MY-FUNCTION
  X = 42
  Y = "hello"
  RESULT = #<unused>
```

Available via `sb-debug:frame-locals` if debug info present.

---

## Source Location

A **Source Location** associates an error or frame with source code:

- File path
- Line number
- Character offset
- Form number

Used to show where in source code an error originated.
