---
type: property
name: package-context
version: 1.0.0
feature: session-management
covers:
  - contracts/session-state
---

# Package Context Property

## Statement

**For all** evaluations in a session,
**if** evaluation E₁ changes the current package (via `in-package` or setting `*package*`),
**then** evaluation E₂ (where E₂ immediately follows E₁) MUST execute in that new package context.

## Formal Expression

```
∀ session S, ∀ adjacent evaluations E₁, E₂ in S :
  (changes_package(E₁, pkg) ∧ success(E₁)) ⟹
    current_package(E₂) = pkg

where changes_package(E, p) ≡
  E contains (in-package p) ∨
  E contains (setf *package* (find-package p))

where current_package(E) ≡
  value of *package* at the start of E
```

## Informal Explanation

Package context persists across evaluations. When you switch packages:

1. **Via in-package**: `(in-package :my-pkg)` changes current package
2. **Via setf**: `(setf *package* (find-package :other))` changes current package
3. **Via tool parameter**: `evaluate-lisp` accepts `package` parameter

The package change affects:
- Symbol resolution in subsequent evaluations
- Where new definitions are interned
- Which symbols are accessible without package prefixes
- Import/use-package effects

Package context is part of persistent session state.

## Rationale

Package context is fundamental to Common Lisp namespacing. Without persistent package context:
- Users cannot work in custom packages
- Symbol resolution would be unpredictable
- Multi-package development would be impossible
- Package-qualified names would be required for everything

This property ensures the session behaves like a true REPL where package switches persist.

## Counterexample Shape

If this property is violated, you might see:
- Switching to `:my-pkg` but next evaluation is still in `:cl-user`
- Defining symbols in a package but they appear in CL-USER
- Package-specific symbols becoming inaccessible
- `*package*` resetting to CL-USER between evaluations
- Different results for same code depending on when it runs

## Verification Approach

**Generator**: Generate package switching sequences

**Assertion**:
```lisp
(defun verify-package-context ()
  ;; E1: Create and switch to new package
  (let ((r1 (evaluate-lisp
             "(defpackage :test-pkg (:use :cl)) (in-package :test-pkg)")))
    (assert (success-p r1)))

  ;; E2: Verify we're still in test-pkg
  (let ((r2 (evaluate-lisp "(package-name *package*)")))
    (assert (success-p r2))
    (assert (equal (extract-result r2) "\"TEST-PKG\"")))

  ;; E3: Define function in test-pkg
  (let ((r3 (evaluate-lisp "(defun local-fn () :in-test-pkg)")))
    (assert (success-p r3)))

  ;; E4: Verify function is in test-pkg, not cl-user
  (let ((r4 (evaluate-lisp "(symbol-package 'local-fn)")))
    (assert (success-p r4))
    (assert (search "TEST-PKG" (extract-result r4))))

  ;; E5: Switch back to CL-USER
  (let ((r5 (evaluate-lisp "(in-package :cl-user)")))
    (assert (success-p r5)))

  ;; E6: Verify we're in CL-USER
  (let ((r6 (evaluate-lisp "(package-name *package*)")))
    (assert (success-p r6))
    (assert (equal (extract-result r6) "\"COMMON-LISP-USER\"")))

  ;; E7: Function needs package qualifier now
  (let ((r7 (evaluate-lisp "(test-pkg:local-fn)")))
    (assert (success-p r7))
    (assert (equal (extract-result r7) ":IN-TEST-PKG"))))
```

**Property Test - Package Parameter Override**:
```lisp
(defun verify-package-parameter ()
  ;; Current package is CL-USER
  (let ((r1 (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result r1) "\"COMMON-LISP-USER\"")))

  ;; Evaluate with explicit package parameter
  (let ((r2 (evaluate-lisp "(defun temp () :test)"
                           :package "KEYWORD")))
    (assert (success-p r2)))

  ;; Package parameter doesn't persist
  (let ((r3 (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result r3) "\"COMMON-LISP-USER\"")))

  ;; In-band package change DOES persist
  (let ((r4 (evaluate-lisp "(in-package :keyword)")))
    (assert (success-p r4)))

  (let ((r5 (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result r5) "\"KEYWORD\""))))
```

**Edge Cases**:
- Creating new packages and switching to them
- Package doesn't exist → PACKAGE-ERROR (but doesn't corrupt state)
- Switching between multiple custom packages
- Using `use-package` and import affects subsequent evaluations
- Package exports/imports persist across evaluations
- Deleting a package (affects subsequent evaluations)

**State Reset**:
After `reset-session`, package context returns to CL-USER:
```lisp
(defun verify-reset-package ()
  ;; Switch to custom package
  (evaluate-lisp "(in-package :keyword)")

  ;; Reset session
  (reset-session)

  ;; Verify back in CL-USER
  (let ((r (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result r) "\"COMMON-LISP-USER\""))))
```

**Shrinking**: Find minimal package switching sequence that loses context

## Implementation Notes

```lisp
;; Package state must be tracked between evaluations
(defvar *session-package* (find-package :cl-user))

(defun evaluate-with-package-context (code &key package)
  (let ((*package* (if package
                       (find-package package)
                       *session-package*)))
    (unwind-protect
         (multiple-value-list (eval (read-from-string code)))
      ;; Capture package changes for next evaluation
      ;; Only if no explicit package parameter was provided
      (unless package
        (setf *session-package* *package*)))))

(defun reset-package-context ()
  (setf *session-package* (find-package :cl-user)))
```

## Relationship to State Persistence

Package context is part of persistent state, but has special characteristics:
- It's a single global value (`*package*`), not a collection of definitions
- It affects symbol resolution for all subsequent code
- It can be temporarily overridden via tool parameters
- It's reset by `reset-session` like other state

Package persistence enables multi-package development workflows in the session.
