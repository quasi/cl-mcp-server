# Code Evaluator Vocabulary

Terms specific to Lisp code evaluation.

---

## Expression

An **Expression** (or form) is a unit of Lisp code to be evaluated. It may be
an atom (symbol, number, string) or a list representing a function call,
special form, or macro invocation.

---

## Multiple Values

Common Lisp functions can return **Multiple Values**. The evaluator must
capture all values, not just the primary one. `(values 1 2 3)` returns
three values.

---

## Read-Eval-Print

The fundamental Lisp interaction loop. The evaluator implements the "Eval"
portion, reading expressions from the request and returning printed results.

---

## Sandbox

A **Sandbox** is a restricted evaluation environment that limits dangerous
operations like file system access, network calls, or system commands.
Implementation is optional but recommended for safety.

---

## Evaluation Timeout

An **Evaluation Timeout** is a safety mechanism that limits how long code
can run before being forcibly interrupted. Prevents infinite loops or
expensive computations from hanging the server indefinitely.

Default: 30 seconds. Configurable via `configure-limits` tool.

When a timeout occurs:
1. The running evaluation is interrupted
2. An `evaluation-timeout` condition is signaled
3. A backtrace is captured showing where execution was
4. The error is formatted with helpful hints

---

## Configurable Limits

**Configurable Limits** are runtime-adjustable parameters that control
evaluation safety:

| Parameter | Default | Purpose |
|-----------|---------|---------|
| `*evaluation-timeout*` | 30 seconds | Maximum evaluation time |
| `*max-output-chars*` | 100,000 | Maximum captured output |

Set timeout to NIL to disable (not recommended for untrusted code).
