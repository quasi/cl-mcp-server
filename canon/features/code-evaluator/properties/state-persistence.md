---
type: property
name: state-persistence
version: 1.0.0
feature: code-evaluator
covers:
  - contracts/evaluate-lisp-tool
relates_to:
  - core/invariants#INV-003
---

# State Persistence Property

## Statement

**For all** evaluations within a session,
**if** an evaluation defines or modifies session state,
**then** that state MUST remain available in all subsequent evaluations
until explicitly modified or session ends.

## Formal Expression

```
∀ session ∈ Sessions :
  ∀ eval₁, eval₂ ∈ session.evaluations :
    eval₁.index < eval₂.index ∧
    defines(eval₁, symbol, value) ⟹
      accessible(eval₂, symbol) ∧
      (¬modified_between(eval₁, eval₂, symbol) ⟹
        lookup(eval₂, symbol) = value)

where defines includes:
  - (defun name args body)      → function definition
  - (defvar name value)          → variable definition
  - (defmacro name args body)    → macro definition
  - (defclass name ...)          → class definition
  - (setf place value)           → variable modification
```

## Informal Explanation

The evaluator provides a persistent REPL session where:

1. **Function definitions persist**: `(defun square (x) (* x x))` makes `square` available forever
2. **Variable definitions persist**: `(defvar *counter* 0)` creates `*counter*` that survives across evaluations
3. **Macro definitions persist**: `(defmacro with-timing ...)` defines macro for future use
4. **Class definitions persist**: `(defclass point ...)` creates class for making instances later
5. **Modifications persist**: `(incf *counter*)` changes state that subsequent code sees
6. **Package state persists**: Package changes via `(in-package)` or `*package*` binding

This is the fundamental REPL experience: build up a workspace incrementally through multiple interactions.

## Rationale

State persistence enables:
- **Incremental development**: Define helper functions, then use them
- **Interactive debugging**: Set variables, test code, adjust and retry
- **Exploratory programming**: Build complex structures piece by piece
- **REPL workflow**: The standard Lisp development experience

Without persistence:
- Every evaluation starts from scratch
- Can't define utilities for reuse
- Must send all dependencies with every evaluation
- Loses the power of interactive development

This property directly implements INV-003 (Session State Persistence).

## Counterexample Shape

If this property is violated, you might see:
- Function defined in eval₁ is undefined in eval₂
- Variable set in eval₁ is unbound in eval₂
- Macro defined in eval₁ is unavailable in eval₂
- Class defined in eval₁ doesn't exist in eval₂
- Package changes don't persist
- Each evaluation appears to start fresh

## Verification Approach

**Property Test - Function Persistence**:

```lisp
(defun test-function-persistence ()
  (let ((session (make-test-session)))
    ;; Define function
    (let ((r1 (evaluate session "(defun square (x) (* x x))")))
      (assert (result-success-p r1))
      (assert (equal "SQUARE" (result-value r1))))

    ;; Use in next evaluation
    (let ((r2 (evaluate session "(square 5)")))
      (assert (result-success-p r2))
      (assert (equal "25" (result-value r2))))

    ;; Use again in third evaluation
    (let ((r3 (evaluate session "(mapcar #'square '(1 2 3))")))
      (assert (result-success-p r3))
      (assert (equal "(1 4 9)" (result-value r3))))))
```

**Property Test - Variable Persistence**:

```lisp
(defun test-variable-persistence ()
  (let ((session (make-test-session)))
    ;; Define variable
    (evaluate session "(defvar *counter* 0)")

    ;; Modify it
    (let ((r1 (evaluate session "(incf *counter*)")))
      (assert (equal "1" (result-value r1))))

    ;; Verify modification persisted
    (let ((r2 (evaluate session "*counter*")))
      (assert (equal "1" (result-value r2))))

    ;; Modify again
    (evaluate session "(incf *counter* 10)")

    ;; Verify new value
    (let ((r3 (evaluate session "*counter*")))
      (assert (equal "11" (result-value r3))))))
```

**Property Test - Macro Persistence**:

```lisp
(defun test-macro-persistence ()
  (let ((session (make-test-session)))
    ;; Define macro
    (evaluate session "(defmacro twice (x) `(+ ,x ,x))")

    ;; Use it
    (let ((result (evaluate session "(twice 21)")))
      (assert (equal "42" (result-value result))))))
```

**Property Test - Class Persistence**:

```lisp
(defun test-class-persistence ()
  (let ((session (make-test-session)))
    ;; Define class
    (evaluate session "(defclass point () ((x :initarg :x) (y :initarg :y)))")

    ;; Make instance
    (evaluate session "(defvar *p* (make-instance 'point :x 3 :y 4))")

    ;; Use in later evaluation
    (let ((result (evaluate session "(slot-value *p* 'x)")))
      (assert (equal "3" (result-value result))))))
```

**Property Test - Package Persistence**:

```lisp
(defun test-package-persistence ()
  (let ((session (make-test-session)))
    ;; Change package
    (evaluate session "(in-package :keyword)")

    ;; Next evaluation should be in that package
    (let ((result (evaluate session "(package-name *package*)")))
      (assert (equal "\"KEYWORD\"" (result-value result))))))
```

**Property Test - Multiple Definitions**:

```lisp
(defun test-multiple-definitions ()
  (let ((session (make-test-session)))
    ;; Build up environment
    (evaluate session "(defun add (a b) (+ a b))")
    (evaluate session "(defun mul (a b) (* a b))")
    (evaluate session "(defvar *x* 10)")
    (evaluate session "(defvar *y* 5)")

    ;; Use everything together
    (let ((result (evaluate session "(add (mul *x* *y*) 2)")))
      (assert (equal "52" (result-value result))))))
```

**Generator**: Generate sequences of definitions and uses:
- Define N functions
- Use those functions in subsequent code
- Modify variables multiple times
- Mix definitions and uses

**Assertion**:
- All definitions remain accessible
- Variable modifications are visible
- No unexpected undefined function/variable errors

**Shrinking**: Find minimal sequence that loses state

**Edge Cases**:
- Redefining same function multiple times
- Shadowing variables with `let`
- Package switching
- Circular definitions
- Very long sessions (100+ evaluations)

## Related

- [definitions scenario](../scenarios/definitions.md) - Test cases for persistence
- [evaluation-isolation property](evaluation-isolation.md) - Session boundaries
