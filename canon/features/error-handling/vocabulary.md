# Error Handling Vocabulary

Terms specific to condition handling and error reporting.

---

## Condition

A **Condition** is Common Lisp's generalized exception mechanism. Unlike
exceptions in other languages, conditions can represent warnings, notes,
and other non-error situations, not just errors.

### Condition Hierarchy

- `condition` - Base class
  - `warning` - Non-fatal issues
    - `style-warning` - Code style issues
  - `serious-condition` - Serious problems
    - `error` - Errors that should be handled
      - `type-error` - Type mismatch
      - `undefined-function` - Missing function
      - `unbound-variable` - Missing binding

---

## Handler

A **Handler** is code that responds to a condition. The server uses handlers
to catch conditions during evaluation and convert them to structured reports.

---

## Restart

A **Restart** is a way to recover from a condition. While the MCP server
typically uses `abort` or `continue` restarts, advanced usage might expose
restarts to Claude for interactive debugging.

---

## Backtrace

A **Backtrace** (or stack trace) shows the call stack at the point a condition
was signaled. Useful for debugging but must be formatted for readability.
