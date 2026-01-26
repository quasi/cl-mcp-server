---
type: property
name: state-persistence
version: 1.0.0
feature: session-management
covers:
  - contracts/session-state
relates_to:
  - core/invariants#INV-003
---

# State Persistence Property

## Statement

**For all** definitions created in evaluation E₁,
**if** the definition completes successfully,
**then** the definition MUST be accessible in all subsequent evaluations E₂, E₃, ... within the same session.

## Formal Expression

```
∀ session S, ∀ evaluations E₁, E₂ where E₁ < E₂ in S :
  (defines(E₁, symbol) ∧ success(E₁)) ⟹
    accessible(E₂, symbol)

where defines(E, s) ≡
  E contains (defun s ...) ∨
  E contains (defvar s ...) ∨
  E contains (defparameter s ...) ∨
  E contains (defconstant s ...) ∨
  E contains (defmacro s ...) ∨
  E contains (defclass s ...) ∨
  E contains (defstruct s ...) ∨
  E contains (defmethod s ...) ∨
  E contains (defgeneric s ...)

where accessible(E, s) ≡
  (fboundp s) ∨ (boundp s) ∨ (find-class s nil) ∨ (macro-function s)
```

## Informal Explanation

When you define something in one evaluation, it must remain available in all later evaluations. This includes:

1. **Functions**: `(defun foo () ...)` makes `foo` callable later
2. **Variables**: `(defvar *x* 10)` makes `*x*` readable/writable later
3. **Macros**: `(defmacro when-debug ...)` makes the macro available later
4. **Classes**: `(defclass point ...)` makes the class available later
5. **Structures**: `(defstruct person ...)` makes the struct and accessors available later
6. **Methods**: CLOS methods remain attached to their generic functions
7. **Generic Functions**: `(defgeneric process ...)` persists across evaluations

This is the core REPL experience: build up functionality incrementally.

## Rationale

INV-003 (Session State Persistence) requires that definitions persist to enable incremental development. Without this property, users cannot:
- Define helper functions and use them later
- Build up complex functionality step-by-step
- Interactively explore and refine code
- Accumulate state in variables across evaluations

This is what makes a REPL useful compared to isolated script execution.

## Counterexample Shape

If this property is violated, you might see:
- Calling a function defined in a previous evaluation produces `UNDEFINED-FUNCTION`
- Referencing a variable defined earlier produces `UNBOUND-VARIABLE`
- A macro defined earlier is not available for expansion
- Classes defined previously cannot be instantiated
- Building on previous definitions fails because they "disappeared"

## Verification Approach

**Generator**: Generate sequences of definitions and uses
```lisp
;; Define in E1
(defun add (a b) (+ a b))

;; Use in E2
(add 1 2)  ; Must return 3, not UNDEFINED-FUNCTION
```

**Assertion**:
```lisp
(defun verify-state-persistence ()
  ;; E1: Define function
  (let ((r1 (evaluate-lisp "(defun test-fn (x) (* x 2))")))
    (assert (success-p r1)))

  ;; E2: Use function
  (let ((r2 (evaluate-lisp "(test-fn 21)")))
    (assert (success-p r2))
    (assert (equal (extract-result r2) "42")))

  ;; E3: Define variable
  (let ((r3 (evaluate-lisp "(defvar *test-var* 100)")))
    (assert (success-p r3)))

  ;; E4: Use variable
  (let ((r4 (evaluate-lisp "*test-var*")))
    (assert (success-p r4))
    (assert (equal (extract-result r4) "100")))

  ;; E5: Define building on previous
  (let ((r5 (evaluate-lisp "(defun test-fn2 (x) (test-fn (+ x 1)))")))
    (assert (success-p r5)))

  ;; E6: Use composite
  (let ((r6 (evaluate-lisp "(test-fn2 20)")))
    (assert (success-p r6))
    (assert (equal (extract-result r6) "42"))))
```

**Property Test**:
- Generate random definition sequences
- For each definition, generate N subsequent evaluations that reference it
- All references must succeed (no UNDEFINED-* errors)
- Values must be consistent with definitions

**Edge Cases**:
- Redefining a function (should update, not lose it)
- Defining multiple interdependent functions
- Defining in custom packages (package context must persist)
- Variables with dynamic values (accumulation across calls)

**Shrinking**: Find minimal definition/use sequence that fails persistence

## Implementation Notes

```lisp
;; The session must maintain state in a persistent environment
;; All evaluations happen in the same Lisp image with shared symbol tables

(defvar *session-package* (find-package :cl-user))

(defun evaluate-in-session (code)
  (let ((*package* *session-package*))
    ;; Evaluate in persistent environment
    (multiple-value-list (eval (read-from-string code)))))

;; Package changes must persist
(defun track-package-changes ()
  (setf *session-package* *package*))
```
