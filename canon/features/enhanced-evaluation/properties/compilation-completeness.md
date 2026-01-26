---
type: property
name: compilation-completeness
version: 0.1.0
feature: enhanced-evaluation
covers:
  - contracts/compile-form-tool
---

# Compilation Completeness Property

## Statement

**For all** code compiled via `compile-form`,
**all compilation diagnostics** (warnings, style-warnings, notes, errors) MUST be captured and reported.

## Formal Expression

```
∀ code ∈ ValidLispCode :
  let diagnostics = compile-form(code).diagnostics
  let actual_conditions = compiler-conditions(code)
  then:
    ∀ c ∈ actual_conditions :
      ∃ d ∈ diagnostics : matches(d, c)

where:
  matches(d, c) ≡
    d.severity = severity(c) ∧
    d.message ⊇ message(c) ∧
    d.form = source-form(c)

  compiler-conditions = {
    errors,           # Compilation errors
    warnings,         # Standard warnings
    style-warnings,   # Style/idiom warnings
    notes,           # Informational notes
    deferred-warnings # End-of-unit warnings
  }
```

## Informal Explanation

The `compile-form` tool must capture all compiler feedback:

1. **Warnings**: Undefined functions, type mismatches, etc.
2. **Style warnings**: Coding style and idiom suggestions
3. **Notes**: Informational messages about optimization decisions
4. **Errors**: Compilation errors (may still compile)
5. **Deferred warnings**: Warnings emitted at end of compilation unit

Each diagnostic includes:
- Severity level (ERROR, WARNING, STYLE-WARNING, NOTE)
- Human-readable message
- Source form that triggered the condition
- Location information when available

This ensures Claude receives complete compilation feedback.

## Rationale

Complete diagnostic capture is essential for:
- Understanding all potential code issues
- Distinguishing between error severities
- Making informed decisions about code quality
- Debugging type and style problems

Missing diagnostics would:
- Hide important warnings
- Lead to runtime surprises
- Reduce code quality
- Waste debugging time

## Counterexample Shape

**Missing Warning**:
```lisp
(compile-form "(defun uses-undefined () (no-such-function))")

;; Returns:
;; "Compilation successful
;;  Warnings: 0"  ; VIOLATION!

;; Should report:
;; "WARNING: Undefined function: NO-SUCH-FUNCTION"
```

**Missing Style Warning**:
```lisp
(compile-form "(defun test (x) (declare (ignore x)) x)")

;; Returns:
;; "Compilation successful
;;  Style-warnings: 0"  ; VIOLATION!

;; Should report:
;; "STYLE-WARNING: Variable X declared IGNORE but used"
```

**Missing Type Note**:
```lisp
(compile-form "(defun generic-add (x y) (+ x y))")

;; Returns:
;; "Compilation successful
;;  Notes: 0"  ; VIOLATION!

;; Should report:
;; "NOTE: Unable to optimize + for unknown types"
```

**Incomplete Deferred Warnings**:
```lisp
(compile-form
  "(defun calls-later () (later-defined))
   (defun later-defined () 42)")

;; Returns:
;; "Compilation successful
;;  Warnings: 1"  ; VIOLATION if warning not specific!

;; Should capture the deferred warning about LATER-DEFINED
;; being undefined at first reference
```

## Verification Approach

**Property Test Strategy**:

1. **Undefined Function Warning**:
   ```lisp
   (let ((result (compile-form "(defun f () (undefined-func))")))
     (assert (> (result-warning-count result) 0))
     (assert (search "UNDEFINED-FUNC" (result-output result))))
   ```

2. **Type Mismatch Warning**:
   ```lisp
   (let ((result (compile-form "(defun bad (x) (+ x \"string\"))")))
     (assert (> (result-warning-count result) 0))
     (assert (search "STRING" (result-output result)))
     (assert (search "NUMBER" (result-output result))))
   ```

3. **Style Warning Capture**:
   ```lisp
   (let ((result (compile-form
                   "(defun unused-arg (x y) (+ x 1))")))
     (assert (> (result-style-warning-count result) 0))
     (assert (search "Y" (result-output result))))
   ```

4. **Compiler Note Capture**:
   ```lisp
   (let ((result (compile-form
                   "(defun generic-math (a b) (+ a b))")))
     ;; May generate optimization notes
     (when (> (result-note-count result) 0)
       (assert (search "NOTE" (result-output result)))))
   ```

5. **Multiple Diagnostics**:
   ```lisp
   (let ((result (compile-form
                   "(defun multi-issue (x)
                      (declare (ignore x))
                      (+ x (undefined-func) \"string\"))")))
     ;; Should capture multiple issues
     (assert (>= (+ (result-warning-count result)
                    (result-style-warning-count result))
                 2)))
   ```

6. **Deferred Warning Capture**:
   ```lisp
   (let ((result (compile-form
                   "(defun early () (late))
                    (defun late () 42)")))
     ;; SBCL may defer the undefined function warning
     ;; with-compilation-unit should catch it
     (assert (or (> (result-warning-count result) 0)
                 ;; Or recognize that LATE is defined later
                 (zerop (result-warning-count result)))))
   ```

**Diagnostic Classification Test**:

```lisp
(defun verify-diagnostic-capture (code expected-severity)
  (let ((result (compile-form code)))
    (and (result-has-diagnostics result)
         (result-has-severity result expected-severity)
         (result-includes-source-form result)
         (result-includes-message result))))
```

**Edge Cases**:

- Reader errors (syntax errors before compilation)
- Package errors (undefined package in package prefix)
- Macro expansion warnings
- Read-time evaluation warnings (`#.` forms)
- Multiple forms with different diagnostic types
- Warnings suppressed by declarations

**Implementation Requirements**:

```lisp
(defun compile-with-diagnostic-capture (code)
  (let ((warnings '())
        (style-warnings '())
        (notes '())
        (errors '()))

    (handler-bind
        ((warning
          (lambda (w)
            (push (format-diagnostic 'warning w) warnings)
            (muffle-warning w)))
         (style-warning
          (lambda (w)
            (push (format-diagnostic 'style-warning w) style-warnings)
            (muffle-warning w)))
         (sb-ext:compiler-note
          (lambda (n)
            (push (format-diagnostic 'note n) notes)
            (muffle-warning n)))
         (error
          (lambda (e)
            (push (format-diagnostic 'error e) errors))))

      (with-compilation-unit (:override t)
        (compile nil `(lambda () ,@(read-forms code)))))

    (format-compilation-results
      warnings style-warnings notes errors)))
```

**Condition Metadata Extraction**:

For each captured condition, extract:
- Condition type and severity
- Error message text
- Source form (if available from condition slots)
- Source location (file, line, column if available)
- Context (enclosing form, function name)

**Shrinking**: Find minimal code that triggers diagnostic but tool fails to capture it
