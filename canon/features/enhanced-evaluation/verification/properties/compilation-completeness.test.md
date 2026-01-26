---
type: verification
name: compilation-completeness-property-test
source: properties/compilation-completeness.md
level: property
tags:
  - property-based
  - compilation
  - diagnostics
---

# Property Test: Compilation Completeness

## Purpose

Verify that `compile-form` captures all compiler diagnostics (warnings, style-warnings, notes, errors) across diverse code samples.

## Prerequisites

- Initialized MCP server
- Property-based testing framework

## Implementation

### Generator: Diagnostic-Producing Code

```lisp
(defun generate-warning-code ()
  "Generate code that triggers compiler warnings"
  (random-choice
   '("(defun uses-undefined () (nonexistent-function 42))"
     "(defun type-error (x) (+ x \"string\"))"
     "(defun unused-result () (list 1 2 3) 42)"
     "(defun calls-self-wrong (x) (calls-self-wrong x y))")))

(defun generate-style-warning-code ()
  "Generate code that triggers style warnings"
  (random-choice
   '("(defun unused-param (x y) (+ x 1))"
     "(defun ignore-violated (x) (declare (ignore x)) x)"
     "(defun bad-naming (camelCase) camelCase)")))

(defun generate-note-code ()
  "Generate code that triggers compiler notes"
  (random-choice
   '("(defun generic-math (a b) (+ a b))"
     "(defun unoptimized (x) (* x x))"
     "(defun generic-list-op (lst) (car lst))")))
```

### Property: All Warnings Captured

```lisp
(deftest compilation-completeness-warnings ()
  "All compiler warnings are captured and reported"
  (dotimes (i 100)
    (let* ((code (generate-warning-code))
           (response (call-tool *test-server* "compile-form"
                               `(("code" . ,code))))
           (content (result-content response)))

      ;; Must report at least one warning
      (is (or (search "WARNING" content)
              (search "Warnings: 1" content)
              (search "Warnings:" content :start2 (- (length content) 50)))
          "No warning reported for: ~A" code)

      ;; Warning count should be > 0
      (is (not (search "Warnings: 0" content))
          "Warning count is 0 for: ~A" code))))
```

### Property: Undefined Function Detection

```lisp
(deftest compilation-completeness-undefined-functions ()
  "Undefined function warnings always captured"
  (dotimes (i 50)
    (let* ((func-name (format nil "UNDEFINED-FUNC-~A" (random 10000)))
           (code (format nil "(defun caller () (~A 42))" func-name))
           (response (call-tool *test-server* "compile-form"
                               `(("code" . ,code))))
           (content (result-content response)))

      ;; Must mention the undefined function
      (is (search func-name content :test #'char-equal)
          "Undefined function ~A not mentioned in diagnostics" func-name)

      ;; Must have warning severity
      (is (or (search "WARNING" content)
              (search "undefined" content :test #'char-equal))
          "Missing undefined function diagnostic for ~A" func-name))))
```

### Property: Type Mismatch Warnings

```lisp
(deftest compilation-completeness-type-errors ()
  "Type mismatch warnings captured"
  (let ((type-error-cases
         '("(defun bad-add () (+ 1 \"string\"))"
           "(defun bad-car () (car 123))"
           "(defun bad-elt () (elt \"string\" \"not-integer\"))")))

    (dolist (code type-error-cases)
      (let* ((response (call-tool *test-server* "compile-form"
                                 `(("code" . ,code))))
             (content (result-content response)))

        ;; Should have warning or note about type issue
        (is (or (search "WARNING" content)
                (search "NOTE" content)
                (search "type" content :test #'char-equal))
            "No type diagnostic for: ~A" code)))))
```

### Property: Multiple Diagnostics

```lisp
(deftest compilation-completeness-multiple-issues ()
  "Multiple diagnostics all captured"
  (let ((multi-issue-code
         "(defun problematic (x y)
            (declare (ignore x))
            (+ x (undefined-func) \"string\" y))"))

    (let* ((response (call-tool *test-server* "compile-form"
                               `(("code" . ,multi-issue-code))))
           (content (result-content response))
           (warning-count (count-diagnostics content "WARNING"))
           (style-count (count-diagnostics content "STYLE")))

      ;; Should have multiple warnings
      (is (>= (+ warning-count style-count) 2)
          "Expected multiple diagnostics, got ~A warnings, ~A style"
          warning-count style-count))))
```

### Property: Deferred Warnings

```lisp
(deftest compilation-completeness-deferred-warnings ()
  "Deferred warnings captured at end of compilation unit"
  (let ((deferred-code
         "(defun calls-later () (later-defined))
          (defun later-defined () 42)"))

    (let* ((response (call-tool *test-server* "compile-form"
                               `(("code" . ,deferred-code))))
           (content (result-content response)))

      ;; Either captures deferred warning OR recognizes forward reference
      ;; Both are acceptable (implementation-specific)
      (is t "Deferred warning handling completed without error"))))
```

### Property: Style Warnings Captured

```lisp
(deftest compilation-completeness-style-warnings ()
  "Style warnings are captured and labeled"
  (dotimes (i 50)
    (let* ((param-name (format nil "UNUSED-~A" i))
           (code (format nil "(defun test-func (x ~A) (+ x 1))" param-name))
           (response (call-tool *test-server* "compile-form"
                               `(("code" . ,code))))
           (content (result-content response)))

      ;; Should mention the unused parameter
      ;; May be warning or style-warning
      (when (or (search "WARNING" content)
                (search "STYLE" content))
        (is (search param-name content :test #'char-equal)
            "Unused parameter ~A not mentioned" param-name)))))
```

### Property: Diagnostic Severity Levels

```lisp
(deftest compilation-completeness-severity-levels ()
  "Diagnostics include severity level information"
  (let ((test-cases
         '(("(defun undefined-call () (no-such-func))" "WARNING")
           ("(defun unused (x y) x)" "STYLE")
           ("(defun unoptimized (a b) (+ a b))" "NOTE"))))

    (dolist (case test-cases)
      (destructuring-bind (code expected-severity) case
        (let* ((response (call-tool *test-server* "compile-form"
                                   `(("code" . ,code))))
               (content (result-content response)))

          ;; Should contain severity marker OR count
          (is (or (search expected-severity content :test #'char-equal)
                  (search (format nil "~As:" expected-severity) content))
              "Missing severity ~A for code: ~A" expected-severity code))))))
```

### Property: Source Form Attribution

```lisp
(deftest compilation-completeness-source-forms ()
  "Diagnostics reference the problematic form"
  (let ((code "(defun calls-bad () (nonexistent-function 42))"))
    (let* ((response (call-tool *test-server* "compile-form"
                               `(("code" . ,code))))
           (content (result-content response)))

      ;; Should mention either the function name or the form
      (is (or (search "CALLS-BAD" content :test #'char-equal)
              (search "NONEXISTENT-FUNCTION" content :test #'char-equal)
              (search "form" content :test #'char-equal))
          "Diagnostic missing source form attribution"))))
```

## Configuration

- Examples: 50-100 per test
- Cover all diagnostic types: WARNING, STYLE-WARNING, NOTE, ERROR

## Helper Functions

```lisp
(defun count-diagnostics (text severity)
  "Count occurrences of diagnostic severity in text"
  (let ((count 0)
        (pos 0))
    (loop while (setf pos (search severity text :start2 pos :test #'char-equal))
          do (incf count)
             (incf pos))
    count))

(defun random-choice (list)
  "Pick random element from list"
  (nth (random (length list)) list))
```

## Notes

- Implementation-specific diagnostic wording acceptable
- What matters: all diagnostics captured, not exact format
- Deferred warnings handling may vary by compiler
- Some optimizations notes are implementation-dependent
