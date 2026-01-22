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
