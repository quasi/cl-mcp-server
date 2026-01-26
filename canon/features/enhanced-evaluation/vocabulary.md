# Enhanced Evaluation Vocabulary

Terms specific to compilation, timing, and performance profiling.

---

## Compilation

**Compilation** is the process of transforming Lisp source code into
executable machine code. The compiler performs:

- Type checking and inference
- Optimization transformations
- Warning/error detection

Compilation can occur without execution, allowing error detection before
code runs.

---

## Compiler Condition

A **Compiler Condition** is a warning, error, or note emitted during
compilation:

| Type | Severity | Description |
|------|----------|-------------|
| `error` | Fatal | Code cannot be compiled |
| `warning` | Non-fatal | Potential problems detected |
| `style-warning` | Stylistic | Code works but violates conventions |
| `note` | Informational | Optimization hints |

---

## Timing Profile

A **Timing Profile** captures execution costs:

- **Real time**: Wall-clock elapsed time
- **Run time**: CPU time spent in user code
- **System time**: CPU time spent in kernel
- **GC time**: Time spent in garbage collection

All times measured in seconds with microsecond precision.

---

## Memory Allocation

**Memory Allocation** tracking measures:

- **Bytes allocated**: Total heap allocation during execution
- **Consing rate**: Bytes allocated per second
- **GC statistics**: Number of collections triggered

SBCL tracks allocation via `sb-ext:get-bytes-consed`.

---

## Package Context

The **Package Context** determines symbol resolution during compilation
and evaluation. Tools accept an optional `package` parameter (defaults
to `CL-USER`).

---

## Compilation Unit

A **Compilation Unit** (via `with-compilation-unit`) batches compilation
warnings and defers undefined function warnings until the unit completes.
Used when compiling related forms together.

---

## TIME Form

The `(time form)` macro in Common Lisp executes code and prints timing
statistics. The `time-execution` tool provides structured access to this
data as JSON.

---

## COMPILE Function

The `(compile name lambda-expression)` function compiles a lambda into
machine code. The `compile-form` tool wraps this to capture compiler
output without side effects.

---

## Lexical Environment

The **Lexical Environment** contains compile-time bindings (macros,
symbol-macros, declarations). Compilation must occur in the correct
lexical environment to match evaluation semantics.
