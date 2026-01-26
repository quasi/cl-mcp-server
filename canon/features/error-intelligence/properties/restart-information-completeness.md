---
type: property
name: restart-information-completeness
version: 0.1.0
feature: error-intelligence
covers:
  - contracts/describe-last-error-tool
relates_to:
  - error-capture-completeness
  - error-state-preservation
---

# Restart Information Completeness Property

## Statement

**For all** errors that occur during evaluation,
**the available restarts** at the error point MUST be completely captured and reported with accurate names and descriptions.

## Formal Expression

```
∀ code ∈ ErrorProducingCode, ∀ error ∈ Conditions :
  let eval_result = evaluate-lisp(code)
  when eval_result.isError = true
  then:
    let actual_restarts = compute-restarts(error)
    let reported_restarts = describe-last-error().restarts

    1. |reported_restarts| = |actual_restarts|
    2. ∀ restart ∈ actual_restarts :
       ∃ reported ∈ reported_restarts :
         reported.name = restart.name ∧
         reported.description ≈ restart.description

    3. At minimum, ABORT restart always present
    4. Restart ordering preserved
    5. Interactive restarts marked as such

where:
  Restart = {
    name: Symbol,
    description: String,
    interactive: Boolean,
    test: Optional[Function]
  }

  compute-restarts returns all available restarts
  from condition handler binding
```

## Informal Explanation

When an error occurs, Common Lisp's condition system provides **restarts** - named recovery options that can be invoked to handle the error. The error intelligence system must capture and report all available restarts accurately.

Restarts include:
1. **System Restarts**: Always present (e.g., ABORT)
2. **User-Defined Restarts**: From RESTART-CASE forms
3. **Interactive Restarts**: Accept user input
4. **Conditional Restarts**: Available based on test function

For each restart, the system must report:
- **Name**: Symbolic name (e.g., ABORT, RETRY, USE-VALUE)
- **Description**: Human-readable explanation
- **Availability**: Which restarts are currently active

This allows Claude to:
- Understand recovery options
- Suggest appropriate error handling
- Explain restart semantics to users
- Recommend best practices for error handling

## Rationale

Complete restart information is essential for:

**Understanding Error Handling**:
- Shows available recovery paths
- Explains Common Lisp condition system
- Demonstrates proper error handling patterns

**Debugging Assistance**:
- Identifies which restarts are available
- Suggests recovery strategies
- Helps understand error context

**Code Quality**:
- Restart presence indicates good error handling
- Missing custom restarts suggest improvement opportunities
- Restart patterns reveal code structure

**User Guidance**:
- Claude can explain what each restart does
- Recommend appropriate restart for situation
- Teach Common Lisp's powerful error recovery

Without complete restart information:
- Lose visibility into recovery options
- Can't suggest proper error handling
- Miss teaching opportunities about condition system
- Incomplete error diagnosis

## Counterexample Shape

If this property is violated:

**Missing User Restarts**:
```lisp
(evaluate-lisp
  "(restart-case (error \"test\")
     (use-zero () 0)
     (use-one () 1))")

;; describe-last-error shows:
{
  "restarts": [
    {"name": "ABORT", "description": "Return to top level"}
    ; VIOLATION! Missing USE-ZERO and USE-ONE
  ]
}
```

**Missing ABORT**:
```lisp
(evaluate-lisp "(error \"simple error\")")

;; describe-last-error shows:
{
  "restarts": []  ; VIOLATION! ABORT should always be present
}
```

**Wrong Restart Names**:
```lisp
(evaluate-lisp
  "(restart-case (/ 1 0)
     (return-zero () 0))")

;; describe-last-error shows:
{
  "restarts": [
    {"name": "RESTART-1", ...}  ; VIOLATION! Should be RETURN-ZERO
  ]
}
```

**Missing Descriptions**:
```lisp
(evaluate-lisp "(error \"test\")")

;; describe-last-error shows:
{
  "restarts": [
    {"name": "ABORT", "description": ""}  ; VIOLATION! No description
  ]
}
```

**Incomplete Restart List**:
```lisp
;; Error with 5 restarts defined
(evaluate-lisp
  "(restart-case
     (restart-case (error \"test\")
       (inner-retry () t))
     (outer-retry () t))")

;; describe-last-error shows only 2 restarts
;; VIOLATION! Missing nested and system restarts
```

**Wrong Restart Order**:
```lisp
(evaluate-lisp
  "(restart-case (error \"x\")
     (first-option () 1)
     (second-option () 2))")

;; Actual order: FIRST-OPTION, SECOND-OPTION, ABORT
;; But describe-last-error shows:
;; ABORT, SECOND-OPTION, FIRST-OPTION
;; VIOLATION! Order matters for semantics
```

## Verification Approach

**Generator**: Generate code with various restart combinations

**Assertion**:
```lisp
(defun verify-restart-completeness (error-code expected-restarts)
  ;; Execute code that will error
  (let ((result (evaluate-lisp error-code)))
    (assert (result-is-error result))

    ;; Get error information
    (let ((error-info (describe-last-error)))

      (and
        ;; Has restarts
        (> (length (error-restarts error-info)) 0)

        ;; Contains expected restarts
        (loop for expected in expected-restarts
              always (find expected
                          (error-restarts error-info)
                          :key #'restart-name
                          :test #'string-equal))

        ;; All restarts have names
        (loop for restart in (error-restarts error-info)
              always (and (restart-name restart)
                         (> (length (restart-name restart)) 0)))

        ;; All restarts have descriptions
        (loop for restart in (error-restarts error-info)
              always (restart-description restart))

        ;; ABORT always present
        (find "ABORT"
              (error-restarts error-info)
              :key #'restart-name
              :test #'string-equal)))))
```

**Property Test Strategy**:

1. **System Restart (ABORT)**:
   ```lisp
   (verify-restart-completeness
     "(error \"simple error\")"
     '("ABORT"))
   ;; ABORT should always be available
   ```

2. **Single User Restart**:
   ```lisp
   (verify-restart-completeness
     "(restart-case (error \"test\")
        (use-value (v) v))"
     '("USE-VALUE" "ABORT"))
   ;; Both custom and system restarts
   ```

3. **Multiple User Restarts**:
   ```lisp
   (verify-restart-completeness
     "(restart-case (error \"test\")
        (use-zero () 0)
        (use-one () 1)
        (use-two () 2))"
     '("USE-ZERO" "USE-ONE" "USE-TWO" "ABORT"))
   ;; All custom restarts plus ABORT
   ```

4. **Nested Restarts**:
   ```lisp
   (verify-restart-completeness
     "(restart-case
        (restart-case (error \"inner\")
          (inner-restart () 'inner))
        (outer-restart () 'outer))"
     '("INNER-RESTART" "OUTER-RESTART" "ABORT"))
   ;; Restarts from multiple levels
   ```

5. **Arithmetic Error Restarts**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (let ((restarts (extract-restarts (describe-last-error))))
     (and
       ;; ABORT always present
       (member "ABORT" restarts :test #'string-equal)
       ;; All have descriptions
       (loop for r in (error-restarts (describe-last-error))
             always (restart-description r))))
   ```

6. **Undefined Function Restarts**:
   ```lisp
   (evaluate-lisp "(undefined-function 42)")
   (let ((restarts (extract-restarts (describe-last-error))))
     ;; Should include RETRY, USE-VALUE, RETURN-VALUE, ABORT
     (and (>= (length restarts) 2)
          (member "ABORT" restarts :test #'string-equal)))
   ```

7. **Type Error Restarts**:
   ```lisp
   (evaluate-lisp "(+ 1 \"string\")")
   (let ((restarts (extract-restarts (describe-last-error))))
     ;; Restarts depend on context but ABORT always there
     (member "ABORT" restarts :test #'string-equal))
   ```

8. **Interactive Restart**:
   ```lisp
   (verify-restart-completeness
     "(restart-case (error \"test\")
        (use-value (v)
          :interactive (lambda () (list (read)))
          v))"
     '("USE-VALUE" "ABORT"))
   ;; Interactive restarts should be captured
   ```

9. **Conditional Restart**:
   ```lisp
   (verify-restart-completeness
     "(restart-case (error \"test\")
        (only-if-true ()
          :test (lambda (c) t)
          'result))"
     '("ONLY-IF-TRUE" "ABORT"))
   ;; Restarts with test functions
   ```

**Edge Cases**:

- **No custom restarts**: Only system restarts (ABORT)
- **Many restarts**: 10+ restarts from nested contexts
- **Restart shadowing**: Inner restart shadows outer same-named
- **Store-value vs use-value**: Standard restart protocols
- **Restart visibility**: Different restarts for different errors
- **Restart descriptions**: May be nil for some system restarts

**Implementation Requirements**:

```lisp
;; Capture restarts at error point
(defun capture-restarts (condition)
  (let ((restarts (compute-restarts condition)))
    (loop for restart in restarts
          collect (list
                   :name (restart-name restart)
                   :description (princ-to-string restart)
                   :interactive (restart-interactive-function restart)
                   :test (restart-test-function restart)))))

;; Format for display
(defun format-restarts (restarts)
  (with-output-to-string (s)
    (format s "~%Available Restarts:~%")
    (loop for restart in restarts
          for i from 1
          do (format s "  ~D. ~A~@[ - ~A~]~%"
                    i
                    (restart-name restart)
                    (restart-description restart)))))
```

**Output Example**:
```
Available Restarts:
  1. USE-ZERO - Use 0 as the result
  2. USE-ONE - Use 1 as the result
  3. ABORT - Return to top level
```

**Shrinking**: Find minimal restart configuration that's incorrectly reported

## Related Properties

- **error-capture-completeness**: Restarts are part of complete error capture
- **error-state-preservation**: Restart list must persist across queries
- **backtrace-accuracy**: Both restarts and backtrace captured together
