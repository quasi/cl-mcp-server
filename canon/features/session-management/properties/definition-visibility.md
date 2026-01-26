---
type: property
name: definition-visibility
version: 1.0.0
feature: session-management
covers:
  - contracts/session-state
relates_to:
  - properties/state-persistence
---

# Definition Visibility Property

## Statement

**For all** user-defined symbols in a session,
**if** the symbol is defined in the current package,
**then** it MUST appear in the output of `list-definitions`.

**For all** symbols in `list-definitions` output,
**if** the symbol is reported as defined,
**then** using that symbol MUST succeed (not produce UNDEFINED-* errors).

## Formal Expression

```
∀ session S, ∀ symbol sym :
  (defined(S, sym) ∧ in_package(S, sym, current_package(S))) ⟹
    appears_in(list_definitions(S), sym)

∀ session S, ∀ symbol sym :
  appears_in(list_definitions(S), sym) ⟹
    ¬undefined_error(use(S, sym))

where defined(S, sym) ≡
  (fboundp sym) ∨ (boundp sym) ∨ (find-class sym nil) ∨ (macro-function sym)

where in_package(S, sym, pkg) ≡
  (symbol-package sym) = pkg ∨ accessible_in(sym, pkg)

where undefined_error(result) ≡
  result.error_type ∈ {UNDEFINED-FUNCTION, UNBOUND-VARIABLE, UNDEFINED-CLASS}
```

## Informal Explanation

The `list-definitions` tool provides a mirror of session state. It must be accurate in both directions:

### Forward Consistency
**If you define it, it appears in the list**

When you define:
- A function → appears in [Functions] section
- A variable → appears in [Variables] section
- A macro → appears in [Macros] section
- A class → appears in [Classes] section
- A loaded system → appears in [Loaded Systems] section

### Backward Consistency
**If it appears in the list, you can use it**

If `list-definitions` shows `FOO` as a function, then `(foo ...)` must work (not UNDEFINED-FUNCTION).
If it shows `*BAR*` as a variable, then `*bar*` must be accessible (not UNBOUND-VARIABLE).

### Scope
The tool shows definitions in the current package and accessible symbols (imported/used).

## Rationale

`list-definitions` is the introspection tool for session state. If it's inconsistent with actual state:
- Users see phantom definitions that don't exist
- Real definitions are hidden from view
- Users cannot trust the tool to understand what's available
- Debugging becomes impossible (reported state differs from actual state)

Consistency between reported state and actual state is critical for usability.

## Counterexample Shape

If this property is violated, you might see:

**Forward Violation** (defined but not listed):
- Define `(defun test () 1)`
- `list-definitions` doesn't show TEST
- But `(test)` still works

**Backward Violation** (listed but not defined):
- `list-definitions` shows FOO as a function
- Calling `(foo)` produces UNDEFINED-FUNCTION error

**Stale State**:
- Define function FOO
- Unintern FOO
- `list-definitions` still shows FOO (stale)

## Verification Approach

**Assertion - Forward Consistency**:
```lisp
(defun verify-forward-consistency ()
  ;; Define various types
  (evaluate-lisp "(defun my-func (x) (* x 2))")
  (evaluate-lisp "(defvar *my-var* 42)")
  (evaluate-lisp "(defmacro my-macro (x) `(list ,x))")
  (evaluate-lisp "(defclass my-class () ())")

  ;; Get listing
  (let ((listing (list-definitions)))
    ;; Each definition must appear
    (assert (search "MY-FUNC" listing))
    (assert (search "*MY-VAR*" listing))
    (assert (search "MY-MACRO" listing))
    (assert (search "MY-CLASS" listing))))
```

**Assertion - Backward Consistency**:
```lisp
(defun verify-backward-consistency ()
  ;; Define some things
  (evaluate-lisp "(defun test-1 () 1)")
  (evaluate-lisp "(defvar *test-2* 2)")

  ;; Parse list-definitions output
  (let* ((listing (list-definitions))
         (functions (extract-functions listing))
         (variables (extract-variables listing)))

    ;; Every listed function must be callable
    (dolist (func functions)
      (let ((result (evaluate-lisp (format nil "(~A)" func))))
        (assert (success-p result))))

    ;; Every listed variable must be accessible
    (dolist (var variables)
      (let ((result (evaluate-lisp (format nil "~A" var))))
        (assert (success-p result))))))
```

**Property Test - Consistency Over Time**:
```lisp
(defun verify-consistency-over-time ()
  ;; Define several symbols
  (let ((symbols '((defun f1 () 1)
                   (defvar *v1* 10)
                   (defmacro m1 () 1))))

    (dolist (form symbols)
      (evaluate-lisp (write-to-string form))

      ;; After each definition, verify consistency
      (verify-forward-consistency)
      (verify-backward-consistency))))
```

**Property Test - Package Scoping**:
```lisp
(defun verify-package-scoping ()
  ;; Define in CL-USER
  (evaluate-lisp "(defun cl-user-func () :cl-user)")

  ;; List shows it
  (let ((listing (list-definitions)))
    (assert (search "CL-USER-FUNC" listing)))

  ;; Switch to new package
  (evaluate-lisp "(defpackage :other (:use :cl))")
  (evaluate-lisp "(in-package :other)")

  ;; List in new package doesn't show cl-user-func
  ;; (unless imported)
  (let ((listing (list-definitions)))
    (assert (not (search "CL-USER-FUNC" listing))))

  ;; Define in new package
  (evaluate-lisp "(defun other-func () :other)")

  ;; List shows new package's definitions
  (let ((listing (list-definitions)))
    (assert (search "OTHER-FUNC" listing))
    (assert (not (search "CL-USER-FUNC" listing)))))
```

**Edge Cases**:
- Empty session → "No user definitions"
- Redefining a function → still appears once (not duplicated)
- Uninterning a symbol → disappears from list
- Shadowing imported symbols → show shadowed version
- Multiple packages → only show current package + accessible
- After reset → list is empty

**Shrinking**: Find minimal definition set that produces inconsistency

## Implementation Notes

```lisp
(defun list-definitions (&key (package *package*))
  (let ((functions '())
        (variables '())
        (macros '())
        (classes '()))

    ;; Collect definitions in current package
    (do-symbols (sym package)
      ;; Only include symbols accessible in this package
      (when (or (eq (symbol-package sym) package)
                (member sym (package-use-list package)))

        (when (fboundp sym)
          (if (macro-function sym)
              (push sym macros)
              (push sym functions)))

        (when (boundp sym)
          (push sym variables))

        (when (find-class sym nil)
          (push sym classes))))

    ;; Format output
    (format-definitions-report functions variables macros classes)))

(defun format-definitions-report (funcs vars macros classes)
  (if (and (null funcs) (null vars) (null macros) (null classes))
      "No user definitions."
      (with-output-to-string (s)
        (when funcs
          (format s "[Functions]~%")
          (dolist (f (sort funcs #'string<))
            (format s "- ~A~%" f)))
        (when vars
          (format s "~%[Variables]~%")
          (dolist (v (sort vars #'string<))
            (format s "- ~A~%" v)))
        (when macros
          (format s "~%[Macros]~%")
          (dolist (m (sort macros #'string<))
            (format s "- ~A~%" m)))
        (when classes
          (format s "~%[Classes]~%")
          (dolist (c (sort classes #'string<))
            (format s "- ~A~%" c))))))
```

## Verification Strategy

This property can be verified through **bidirectional checking**:

1. **After every `evaluate-lisp` that defines something**:
   - Run `list-definitions`
   - Verify the new symbol appears

2. **For every symbol in `list-definitions` output**:
   - Attempt to use it
   - Verify no UNDEFINED-* error

3. **Property test**: Generate random definition sequences, verify consistency at each step

This ensures the introspection tool accurately reflects reality.
