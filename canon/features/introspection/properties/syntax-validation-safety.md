---
type: property
name: syntax-validation-safety
version: 0.1.0
feature: introspection
covers:
  - contracts/validate-syntax-tool
relates_to:
  - scenarios/syntax-validation
  - properties/read-only-guarantee
---

# Syntax Validation Safety Property

## Statement

**For all** input strings provided to `validate-syntax`,
**the tool** MUST safely validate without executing code, crashing, or modifying state, regardless of input validity or maliciousness.

## Formal Expression

```
∀ code_string ∈ Strings, ∀ state₀ ∈ SessionState :
  let (result, state₁) = validate-syntax(code_string, state₀)
  then:
    (result ∈ {valid, invalid}) ∧
    observable_state(state₁) = observable_state(state₀) ∧
    no_code_executed(code_string) ∧
    no_crash

where:
  no_code_executed(s) ≡
    ∀ form ∈ (read-forms s) : ¬evaluated(form)

  no_crash ≡
    validate-syntax terminates normally (returns result or error message)
```

## Informal Explanation

The `validate-syntax` tool must be completely safe to use on **any** input:

1. **No Execution**: Never evaluate any code from the input
2. **No State Modification**: No side effects (same as read-only guarantee)
3. **No Crashes**: Handle all malformed input gracefully
4. **Deterministic**: Same input always produces same result
5. **Bounded**: Terminates in reasonable time even on pathological input

This is critical because Claude may validate:
- Partially edited code (incomplete, malformed)
- Untrusted code from external sources
- Adversarial input designed to exploit readers

The tool must be a **safe sandbox** for syntax checking.

## Rationale

Syntax validation happens **before** code execution, often on incomplete or malformed code. It must be safe because:

1. **Pre-save Workflow**: Validate before writing to files
2. **Incremental Editing**: Check partially complete code
3. **User Protection**: Don't execute untrusted code during validation
4. **No Surprises**: Users expect validation to be side-effect-free

Violations could cause:
- Accidental code execution (security risk)
- State corruption (breaks session)
- Crashes (disrupts workflow)
- Unbounded resource usage (DOS)

## Counterexample Shape

If this property is violated, you might see:

**Code Execution During Validation**:
```lisp
;; Malicious input
"(defparameter *hacked* (delete-file \"important.lisp\"))"

;; Wrong behavior
validate-syntax(input)
;; VIOLATION! File gets deleted during "validation"

;; Correct behavior
validate-syntax(input) → "✓ Syntax valid: 1 form"
;; File not touched
```

**Reader Macro Exploitation**:
```lisp
;; Input with reader macro that has side effects
"#.(delete-all-files)"

;; Wrong behavior
validate-syntax(input)
;; VIOLATION! Reader evaluates #. form during read

;; Correct behavior
validate-syntax(input) → "✗ Syntax invalid: Error: #. reader macro disabled"
```

**Crash on Malformed Input**:
```lisp
;; Deeply nested structure (DOS attempt)
"(((((((((((...)))))))))))))"  ; thousands of levels

;; Wrong behavior
validate-syntax(input)
;; VIOLATION! Stack overflow, process crashes

;; Correct behavior
validate-syntax(input) → "✗ Syntax invalid: Excessive nesting"
```

**Infinite Loop**:
```lisp
;; Circular structure in input
"#1=#1#"

;; Wrong behavior
validate-syntax(input)
;; VIOLATION! Hangs forever trying to read

;; Correct behavior
validate-syntax(input) → "✗ Syntax invalid: Circular reference"
```

## Verification Approach

**Generator**: Generate both valid and pathological input strings

**Assertion**:
```lisp
(defun verify-syntax-validation-safety (input-string)
  ;; Capture state before
  (let ((state-before (capture-state))
        (files-before (list-files)))

    ;; Validate with timeout
    (let ((result (with-timeout 5
                    (validate-syntax input-string))))

      ;; Verify safety properties
      (and (not (null result))              ; didn't crash/hang
           (equal state-before (capture-state))  ; no state change
           (equal files-before (list-files))     ; no file operations
           (valid-result-format-p result)))))    ; proper result
```

**Property Test Strategy**:

1. **No Execution Test**:
   ```lisp
   (defun test-no-execution ()
     ;; Code with side effects
     (let ((sentinel nil))
       (let ((code "(setf *test-sentinel* t)"))
         (validate-syntax code)

         ;; Verify code wasn't executed
         (assert (not (boundp '*test-sentinel*))))))
   ```

2. **Reader Macro Safety Test**:
   ```lisp
   (defun test-reader-macro-safety ()
     ;; Dangerous reader macros must be disabled
     (let ((dangerous-inputs
             '("#.(delete-file \"test\")"
               "#.(quit)"
               "#.(sb-ext:run-program \"rm\" '(\"-rf\" \"/\"))")))

       (dolist (input dangerous-inputs)
         ;; Should handle safely, not execute
         (let ((result (validate-syntax input)))
           (assert (search "invalid" result))
           ;; Verify system still intact
           (assert (probe-file "test.lisp"))))))
   ```

3. **Malformed Input Test**:
   ```lisp
   (defun test-malformed-input-handling ()
     ;; Various types of malformed input
     (let ((malformed-inputs
             '("((((("                    ; unclosed parens
               ")))))"                    ; extra close parens
               "(defun"                   ; truncated
               "\""                       ; unterminated string
               "#\\xFFFFFF"               ; invalid character literal
               "(lambda (x) (+ x")))      ; incomplete

       (dolist (input malformed-inputs)
         ;; Should return invalid, not crash
         (let ((result (validate-syntax input)))
           (assert (search "invalid" result))))))
   ```

4. **DOS Attack Test**:
   ```lisp
   (defun test-dos-protection ()
     ;; Pathological inputs designed to cause problems
     (let ((pathological-inputs
             `(,(make-string 1000000 :initial-element #\()  ; million open parens
               ,(format nil "(~{~A ~})" (loop repeat 100000 collect 'x))  ; huge list
               "#1=(#1#)")))                                  ; circular ref

       (dolist (input pathological-inputs)
         ;; Should handle with timeout/limits
         (let ((result (with-timeout 5
                         (validate-syntax input))))
           (assert result "Should not hang")
           (assert (or (search "invalid" result)
                       (search "valid" result)))))))
   ```

5. **State Isolation Test**:
   ```lisp
   (defun test-state-isolation ()
     ;; Validate code that would modify state
     (defparameter *test-counter* 0)

     (let ((code "(incf *test-counter*)"))
       (validate-syntax code)

       ;; Counter should be unchanged
       (assert (= *test-counter* 0))))
   ```

6. **Package Safety Test**:
   ```lisp
   (defun test-package-safety ()
     ;; Code that would modify packages
     (let ((initial-packages (list-all-packages))
           (code "(defpackage :new-package (:use :cl))"))

       (validate-syntax code)

       ;; Package should not be created
       (assert (equal initial-packages (list-all-packages)))))
   ```

7. **File System Safety Test**:
   ```lisp
   (defun test-filesystem-safety ()
     ;; Code with file operations
     (let ((code "(with-open-file (s \"test.txt\" :direction :output)
                    (write-line \"data\" s))"))

       (validate-syntax code)

       ;; File should not be created
       (assert (not (probe-file "test.txt")))))
   ```

8. **Bounded Resource Test**:
   ```lisp
   (defun test-bounded-resources ()
     ;; Verify validation completes quickly
     (let ((code (make-large-valid-form 10000)))  ; 10k forms

       (let ((start (get-internal-real-time)))
         (validate-syntax code)
         (let ((elapsed (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second)))

           ;; Should complete in reasonable time
           (assert (< elapsed 5) "Validation too slow")))))
   ```

**Edge Cases**:
- Empty string (valid: 0 forms)
- Unicode characters in strings/comments
- Mixed valid and invalid forms in same input
- Very large integers (could cause memory issues)
- Pathological macro characters
- Escape sequences in strings

**Implementation Requirements**:

The tool MUST:
```lisp
;; 1. Disable dangerous reader features
(let ((*read-eval* nil)              ; Disable #. reader macro
      (*print-readably* nil)         ; Prevent read-time errors
      (*read-suppress* nil))         ; We want to actually read

  ;; 2. Use safe reading with error handling
  (handler-case
      (with-input-from-string (stream code)
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              count t))

    ;; 3. Catch all reader errors
    (reader-error (e)
      (format nil "Syntax invalid: ~A" e))
    (error (e)
      (format nil "Syntax invalid: ~A" e))))
```

Must NOT do:
```lisp
(eval (read-from-string code))       ; NEVER eval during validation
(compile nil `(lambda () ,code))     ; Don't compile either
```

**Timeout Protection**:
```lisp
(defun validate-with-timeout (code timeout-seconds)
  (with-timeout timeout-seconds
    (validate-syntax-internal code)
    :timeout-exceeded))
```

**Shrinking**: Find minimal input that causes crash, execution, or state change
