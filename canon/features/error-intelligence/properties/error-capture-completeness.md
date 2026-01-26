---
type: property
name: error-capture-completeness
version: 0.1.0
feature: error-intelligence
covers:
  - contracts/describe-last-error-tool
  - contracts/get-backtrace-tool
---

# Error Capture Completeness Property

## Statement

**For all** errors that occur during evaluation,
**the error intelligence tools** MUST capture complete error information including type, message, backtrace, and available restarts.

## Formal Expression

```
∀ code ∈ ErrorProducingCode, ∀ error ∈ Conditions :
  let eval_result = evaluate-lisp(code)
  when eval_result.isError = true
  then:
    1. describe-last-error() returns complete ErrorContext
    2. get-backtrace() returns complete StackTrace
    3. ErrorContext contains:
       - error.type (condition class)
       - error.message (formatted message)
       - error.restarts (available recovery options)
    4. StackTrace contains:
       - All frames from error point to top level
       - Function names for each frame
       - Arguments (if available)

where:
  ErrorContext = {
    type: String,
    message: String,
    restarts: [Restart],
    source_location: Optional[Location]
  }

  StackTrace = [Frame] where Frame = {
    number: Integer,
    function: String,
    arguments: Optional[List],
    source: Optional[Location]
  }
```

## Informal Explanation

When errors occur, the system must preserve all diagnostic information:

1. **Error Type**: The condition class (e.g., `DIVISION-BY-ZERO`)
2. **Error Message**: Human-readable description
3. **Backtrace**: Complete call stack from error to top level
4. **Restarts**: Recovery options available at error point
5. **Source Location**: Where error originated (if available)

This allows Claude to fully understand and diagnose failures.

## Rationale

Complete error information is essential for:
- Debugging user code
- Understanding failure causes
- Suggesting fixes
- Learning about error patterns

Missing information would:
- Hamper debugging efforts
- Lead to incorrect diagnoses
- Frustrate users

Key capture requirements:
- Errors must be caught without suppressing
- Backtrace must be captured at error point
- Restart information must be preserved
- Multiple errors should each be tracked

## Counterexample Shape

If this property is violated:

**Missing Backtrace**:
```lisp
;; Error occurs in nested function
(evaluate-lisp "(defun a () (/ 1 0)) (defun b () (a)) (b)")

;; describe-last-error shows:
{
  "type": "DIVISION-BY-ZERO",
  "message": "arithmetic error",
  "backtrace": []  ; VIOLATION! Empty backtrace
}
```

**Missing Restart Information**:
```lisp
(evaluate-lisp "(error 'simple-error)")

;; Available restarts: ABORT, RETRY
;; But describe-last-error shows:
{
  "restarts": []  ; VIOLATION! Restarts not captured
}
```

**Lost Error Type**:
```lisp
(evaluate-lisp "(/ 1 0)")

;; describe-last-error shows:
{
  "type": "ERROR",  ; VIOLATION! Too generic, should be DIVISION-BY-ZERO
  "message": "An error occurred"
}
```

**Incomplete Stack Frames**:
```lisp
(evaluate-lisp "(defun deep () (error \"oops\")) (deep)")

;; get-backtrace() shows:
[
  { "frame": 0, "function": "ERROR" }
  ; VIOLATION! Missing DEEP and EVAL frames
]
```

## Verification Approach

**Generator**: Generate code that produces various error types

**Assertion**:
```lisp
(defun verify-error-capture (error-code)
  ;; Execute code that will error
  (let ((result (evaluate-lisp error-code)))
    (assert (result-is-error result))

    ;; Verify error information captured
    (let ((error-info (describe-last-error))
          (backtrace (get-backtrace)))

      (and
        ;; Error type is specific (not just "ERROR")
        (not (string= (error-type error-info) "ERROR"))

        ;; Message is non-empty
        (> (length (error-message error-info)) 0)

        ;; Backtrace has frames
        (> (length backtrace) 0)

        ;; At least one restart available
        (> (length (error-restarts error-info)) 0)

        ;; First frame is the error point
        (position "ERROR" (frame-function (first backtrace))
                 :test #'search)))))
```

**Property Test Strategy**:

1. **Arithmetic Errors**:
   ```lisp
   (verify-error-capture "(/ 1 0)")
   ;; Should show DIVISION-BY-ZERO with full stack
   ```

2. **Type Errors**:
   ```lisp
   (verify-error-capture "(+ 1 \"string\")")
   ;; Should show TYPE-ERROR with argument info
   ```

3. **Undefined Function**:
   ```lisp
   (verify-error-capture "(nonexistent-function 42)")
   ;; Should show UNDEFINED-FUNCTION with symbol name
   ```

4. **User Errors**:
   ```lisp
   (verify-error-capture "(error \"Custom error\")")
   ;; Should show SIMPLE-ERROR with custom message
   ```

5. **Nested Calls**:
   ```lisp
   (verify-error-capture
     "(defun a () (/ 1 0))
      (defun b () (a))
      (defun c () (b))
      (c)")
   ;; Backtrace should show: / -> A -> B -> C -> EVAL
   ```

6. **Restart Availability**:
   ```lisp
   ;; Execute with custom restarts
   (verify-error-capture
     "(restart-case (error \"test\")
        (use-zero () 0)
        (use-one () 1))")
   ;; Should capture USE-ZERO and USE-ONE restarts
   ```

**Edge Cases**:

- Errors in compiler (during compilation)
- Reader errors (syntax problems)
- Stack overflow (very deep call chains)
- Errors in error handling code
- Multiple sequential errors

**Implementation Requirements**:

```lisp
;; Error capture in evaluation
(defun safe-evaluate (code)
  (handler-bind
    ((error
      (lambda (condition)
        ;; Capture full context at error point
        (setf *last-error*
          (make-error-context
            :type (type-of condition)
            :message (princ-to-string condition)
            :backtrace (sb-debug:list-backtrace)
            :restarts (compute-restarts condition))))))
    (eval (read-from-string code))))
```

**Shrinking**: Find minimal code that loses error information

## Related Properties

- **backtrace-accuracy**: Stack frames match actual calls
- **restart-information-accuracy**: Restarts are correctly identified
- **error-history-consistency**: Error tracking across evaluations
