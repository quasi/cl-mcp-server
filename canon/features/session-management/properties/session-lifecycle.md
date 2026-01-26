---
type: property
name: session-lifecycle
version: 1.0.0
feature: session-management
covers:
  - contracts/session-state
---

# Session Lifecycle Property

## Statement

**For all** sessions,
**if** the session is reset via `reset-session`,
**then** the session MUST return to its initial state equivalent to a fresh session.

## Formal Expression

```
∀ session S, ∀ state before_reset, after_reset :
  (state(S) = before_reset ∧ reset(S)) ⟹
    state(S) = initial_state ∧
    state(S) ≡ state(new_session())

where initial_state ≡
  { user_definitions: ∅,
    current_package: CL-USER,
    user_packages: ∅,
    loaded_systems: preserved }

where user_definitions ≡
  functions ∪ variables ∪ macros ∪ classes ∪ structures ∪ methods

where preserved ≡
  systems loaded via quickload/load-system remain in image
  (cannot be unloaded without restarting Lisp process)
```

## Informal Explanation

Session lifecycle has three phases:

### 1. Initialization (Fresh Session)
When the server starts:
- Current package is CL-USER
- No user-defined symbols exist
- No custom packages exist (only CL standard packages)
- Loaded systems: only server dependencies

### 2. Accumulation (Active Session)
During use:
- User definitions accumulate
- Package context may change
- Custom packages may be created
- Additional systems may be loaded

### 3. Reset (Return to Fresh)
After `reset-session`:
- All user-defined symbols are removed
- Custom packages are deleted
- Current package returns to CL-USER
- Loaded systems remain (cannot be unloaded)

**Invariant**: `reset-session` produces a state functionally equivalent to a fresh session, except for loaded systems which cannot be unloaded.

## Rationale

Users need the ability to start fresh without restarting the server:
- Clean slate after experimentation
- Recover from namespace pollution
- Start new development without old definitions interfering
- Test that code doesn't depend on stale state

Reset must be thorough: any leftover definitions would violate the "fresh session" guarantee.

## Counterexample Shape

If this property is violated, you might see:
- Functions still defined after reset
- Variables still bound after reset
- Custom packages still existing after reset
- Package context not returning to CL-USER
- Definitions "leaking" across reset boundary
- Reset being partial or incomplete

## Verification Approach

**Assertion - Full Reset Cycle**:
```lisp
(defun verify-session-lifecycle ()
  ;; Phase 1: Fresh session state
  (assert-fresh-session)

  ;; Phase 2: Accumulate state
  (evaluate-lisp "(defun test-fn (x) (* x 2))")
  (evaluate-lisp "(defvar *test-var* 100)")
  (evaluate-lisp "(defmacro test-mac (x) `(+ ,x 1))")
  (evaluate-lisp "(defclass test-cls () ())")
  (evaluate-lisp "(defpackage :test-pkg (:use :cl))")
  (evaluate-lisp "(in-package :test-pkg)")

  ;; Verify state exists
  (let ((defs (list-definitions)))
    (assert (search "TEST-FN" defs))
    (assert (search "*TEST-VAR*" defs))
    (assert (search "TEST-MAC" defs))
    (assert (search "TEST-CLS" defs)))

  (let ((pkg (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result pkg) "\"TEST-PKG\"")))

  ;; Phase 3: Reset
  (reset-session)

  ;; Verify returned to fresh state
  (assert-fresh-session))

(defun assert-fresh-session ()
  ;; No user definitions
  (let ((defs (list-definitions)))
    (assert (search "No user definitions" defs)))

  ;; Package is CL-USER
  (let ((pkg (evaluate-lisp "(package-name *package*)")))
    (assert (equal (extract-result pkg) "\"COMMON-LISP-USER\"")))

  ;; Previous definitions are gone
  (let ((r1 (evaluate-lisp "(test-fn 21)")))
    (assert (error-p r1))
    (assert (search "UNDEFINED-FUNCTION" (extract-error r1))))

  (let ((r2 (evaluate-lisp "*test-var*")))
    (assert (error-p r2))
    (assert (search "UNBOUND-VARIABLE" (extract-error r2))))

  (let ((r3 (evaluate-lisp "(find-package :test-pkg)")))
    (assert (success-p r3))
    (assert (equal (extract-result r3) "NIL"))))
```

**Property Test - Reset Idempotence**:
```lisp
(defun verify-reset-idempotence ()
  ;; Create some state
  (evaluate-lisp "(defun foo () 1)")

  ;; Reset once
  (let ((r1 (reset-session)))
    (assert (success-p r1)))

  ;; Reset again (should be no-op, same result)
  (let ((r2 (reset-session)))
    (assert (success-p r2)))

  ;; State is still fresh
  (assert-fresh-session))
```

**Property Test - Reset is Complete**:
```lisp
(defun verify-reset-completeness ()
  ;; Create diverse state
  (evaluate-lisp "(defun f1 () 1)")
  (evaluate-lisp "(defun f2 () 2)")
  (evaluate-lisp "(defvar *v1* 10)")
  (evaluate-lisp "(defvar *v2* 20)")
  (evaluate-lisp "(defmacro m1 () 1)")
  (evaluate-lisp "(defclass c1 () ())")
  (evaluate-lisp "(defpackage :p1 (:use :cl))")
  (evaluate-lisp "(defpackage :p2 (:use :cl))")

  ;; Reset
  (reset-session)

  ;; EVERY definition must be gone
  (dolist (symbol '(f1 f2 *v1* *v2* m1 c1))
    (assert-symbol-undefined symbol))

  (dolist (pkg '(:p1 :p2))
    (assert (null (find-package pkg)))))
```

**Edge Cases**:
- Reset with no accumulated state (no-op)
- Reset multiple times in a row (idempotent)
- Reset after loading systems (systems remain loaded)
- Reset after defining in multiple packages (all user packages cleared)
- Reset after errors (still completes reset)
- Complex interdependent definitions (all cleared)

**Systems Persistence After Reset**:
```lisp
(defun verify-loaded-systems-persist ()
  ;; Load a system
  (load-system "alexandria")
  (assert (find-package :alexandria))

  ;; Use it
  (evaluate-lisp "(alexandria:hash-table-keys (make-hash-table))")

  ;; Reset session
  (reset-session)

  ;; System package still exists (loaded systems can't be unloaded)
  (assert (find-package :alexandria))

  ;; Can still use system functions
  (let ((r (evaluate-lisp
            "(alexandria:hash-table-keys (make-hash-table))")))
    (assert (success-p r))))
```

**Shrinking**: Find minimal state that survives reset incorrectly

## Implementation Notes

```lisp
(defun reset-session ()
  ;; 1. Unintern all symbols in CL-USER
  (do-symbols (sym (find-package :cl-user))
    (when (eq (symbol-package sym) (find-package :cl-user))
      (unintern sym :cl-user)))

  ;; 2. Delete user-created packages (not standard CL packages)
  (dolist (pkg (list-all-packages))
    (let ((name (package-name pkg)))
      (when (and (not (member name *standard-packages* :test #'string=))
                 (not (search "COMMON-LISP" name)))
        (delete-package pkg))))

  ;; 3. Reset package context
  (setf *package* (find-package :cl-user))

  ;; 4. Clear session tracking state
  (clrhash *session-definitions*)

  ;; Note: Cannot unload systems - they remain in image
  ;; This is a Lisp limitation, not a bug

  "Session reset. All definitions cleared.\nCurrent package: CL-USER")

(defvar *standard-packages*
  '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD"
    "CL" "CL-USER" "ASDF" "UIOP" "QUICKLISP"))
```

## Relationship to Other Properties

- **State Persistence**: Reset is the inverse operation - it removes persistent state
- **Package Context**: Reset restores default package context
- **Definition Visibility**: After reset, no user definitions are visible

Reset is the controlled way to clear session state without server restart.
