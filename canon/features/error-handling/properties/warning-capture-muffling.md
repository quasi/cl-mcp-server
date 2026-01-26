---
type: property
name: warning-capture-muffling
version: 1.0.0
feature: error-handling
covers:
  - contracts/condition-report
---

# Warning Capture and Muffling Property

## Statement

**For all** code that signals warnings during evaluation,
**the warnings** MUST be captured and included in the response,
**the evaluation** MUST continue to completion (warnings are non-fatal),
AND **the response** MUST NOT set `isError: true`.

## Formal Expression

```
∀ code ∈ WarningProducingCode :
  let response = evaluate(code) in
    response.isError = false ∧
    has_section(response, "warnings") ∧
    warnings_captured(response) ∧
    has_result_value(response) ∧
    evaluation_completed(code)

where warnings_captured(response) ≡
  ∀ warning ∈ warnings_signaled_by(code) :
    response.text contains warning.type ∧
    response.text contains warning.message
```

## Informal Explanation

Warnings are non-fatal conditions that should not stop evaluation. The server must:

1. **Capture warnings**: Use `handler-bind` with `warning` type to intercept
2. **Muffle warnings**: Call `muffle-warning` to prevent propagation to stderr
3. **Continue evaluation**: Let code run to completion despite warnings
4. **Report warnings**: Include captured warnings in response text
5. **Include result**: Show the computed value after warnings
6. **Mark as success**: Set `isError: false` since warnings are not errors

Warning response format:
```
[warnings]
STYLE-WARNING: Using deprecated function OLD-FN.
WARNING: Undefined variable assumed special.

=> {result-value}
```

This differs from errors, which:
- Stop evaluation immediately
- Set `isError: true`
- Show backtrace instead of result

## Rationale

Warnings indicate potential issues but should not prevent code execution:
- **Style warnings**: Code works but could be improved
- **Compiler warnings**: Optimization notices or minor issues
- **Deprecation warnings**: Feature still works but is discouraged

Stopping on warnings would be too strict and interrupt normal development. Users need to see:
- What warnings occurred (for awareness)
- What the code computed (the result)
- That evaluation succeeded (not an error)

By capturing and muffling warnings, the server provides useful feedback without polluting stderr or stopping execution.

## Counterexample Shape

If this property is violated, you might see:

**Evaluation Stopped**:
```lisp
(let (x) x)  ; signals STYLE-WARNING
```
Server returns error instead of completing evaluation.

**Warning Propagates to stderr**:
```
WARNING: redefining COMMON-LISP-USER::FOO in DEFUN
```
This appears in server stderr instead of being captured in response.

**Warning Marked as Error**:
```json
{
  "content": [{"type": "text", "text": "[warnings]\nWARNING: ...\n\n=> 42"}],
  "isError": true
}
```
The `isError: true` is wrong - warnings are not errors.

**Warning Missing from Response**:
```json
{
  "content": [{"type": "text", "text": "=> 42"}],
  "isError": false
}
```
Warning was signaled but not captured in response.

**No Result Value**:
```json
{
  "content": [{"type": "text", "text": "[warnings]\nWARNING: ..."}],
  "isError": false
}
```
Warnings shown but result value omitted.

## Verification Approach

**Warning Generators**: Code that signals various warning types

```lisp
(defvar *warning-generators*
  '(;; Style warnings
    ("(let (x) x)"
     . "unused variable")

    ("(lambda (x) 42)"
     . "unused variable")

    ;; Redefinition warnings
    ("(defun test-fn () 1) (defun test-fn () 2)"
     . "redefining")

    ;; Undefined variable warnings (some implementations)
    ("(defun uses-special () *undefined-var*)"
     . "undefined variable")

    ;; Deprecation warnings (if available)
    ;; Implementation-specific examples

    ;; Compiler warnings
    ("(compile nil '(lambda (x) (+ x \"string\")))"
     . "type mismatch")))
```

**Assertion Protocol**:
```lisp
(defun verify-warning-capture (code expected-warning-text)
  (let ((response (evaluate-lisp code)))
    ;; Response should be success (not error)
    (assert (not (error-response-p response))
            nil
            "Warning marked as error for: ~A" code)

    (assert (result-response-p response)
            nil
            "Warning response malformed for: ~A" code)

    (let ((text (response-text response)))
      ;; Should have warnings section
      (assert (search "[warnings]" text)
              nil
              "Missing [warnings] section for: ~A" code)

      ;; Should contain warning text
      (assert (or (search expected-warning-text text)
                  (search (string-upcase expected-warning-text) text))
              nil
              "Warning text not found in: ~A" text)

      ;; Should have result marker
      (assert (search "=>" text)
              nil
              "Missing result marker in warning response: ~A" text)

      ;; Warnings should come before result
      (assert (< (search "[warnings]" text) (search "=>" text))
              nil
              "Result appears before warnings in: ~A" text))))
```

**Muffling Verification**:
```lisp
(defun verify-warning-muffled (code)
  "Verify warning doesn't leak to stderr"
  (let ((stderr-output nil))
    ;; Capture stderr
    (unwind-protect
         (progn
           (setf stderr-output
                 (with-output-to-string (*error-output*)
                   (evaluate-lisp code)))

           ;; Verify stderr is empty or only has expected output
           (assert (or (zerop (length stderr-output))
                       ;; Allow non-warning output
                       (not (search "WARNING" stderr-output)))
                   nil
                   "Warning leaked to stderr: ~A" stderr-output)))))
```

**Property Test**:
1. For each warning generator:
   - Send evaluation request
   - Verify `isError: false`
   - Verify `[warnings]` section present
   - Verify warning type and message captured
   - Verify result value present after warnings
   - Verify evaluation completed
2. Test multiple warnings in single evaluation
3. Test warnings combined with stdout output
4. Verify warnings don't appear in stderr

**Multiple Warnings Test**:
```lisp
(verify-multiple-warnings
  "(let (x y z) (+ 1 1))"
  :expected-count 3
  :expected-types '("unused variable" "unused variable" "unused variable"))
```

**Warnings + Output Test**:
```lisp
(verify-warning-with-output
  "(progn (format t \"hello~%\") (let (x) 42))"
  :expected-sections '("[stdout]" "[warnings]" "=>")
  :section-order '(:stdout :warnings :result))
```

**Edge Cases**:
- Code that signals multiple warnings (verify all captured)
- Code that signals warnings then errors (error takes precedence)
- Nested warnings (warning during warning handling)
- Very long warning messages (test truncation)
- Warning messages with special characters
- Warnings from compiled vs. interpreted code
- Warnings from macroexpansion
- Implementation-specific warning types

**Format Invariant**:
```lisp
(defun verify-warning-format (response)
  (let ((text (response-text response)))
    ;; Must not be marked as error
    (assert (not (response-is-error-p response)))

    ;; If has warnings section, must have result
    (when (search "[warnings]" text)
      (assert (search "=>" text))
      (assert (< (search "[warnings]" text) (search "=>" text))))))
```

**Regression Tests**: Known warning cases
```lisp
(define-warning-test unused-variable
  :code "(let (x) 42)"
  :expect-warning "unused"
  :expect-result "42")

(define-warning-test redefinition
  :code "(defun tmp-fn () 1) (defun tmp-fn () 2)"
  :expect-warning "redefining"
  :expect-result "TMP-FN")
```

**Shrinking**: Find minimal warning-producing code that violates muffling or capture
