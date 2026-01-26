---
type: verification
name: condition-handling-property-test
source: properties/condition-handling.md
level: property
tags:
  - property-based
  - error-handling
---

# Property Test: Condition Handling

## Purpose

Verify that all conditions (errors, warnings, serious-conditions) are gracefully handled without crashing the server, with proper type preservation and backtrace capture.

## Prerequisites

- Property-based testing framework
- Test session with clean state

## Implementation

### Generator: Random Error-Producing Code

```lisp
(defun generate-error-code ()
  "Generate code that signals various errors"
  (random-choice
   (list
    ;; Arithmetic errors
    "(/ 1 0)"
    "(mod 10 0)"

    ;; Type errors
    "(car 42)"
    "(+ \"string\" 1)"
    "(length 123)"
    "(1+ 'symbol)"

    ;; Undefined references
    "undefined-variable"
    "(undefined-function)"
    "(funcall 'not-a-function)"

    ;; Explicit errors
    "(error \"test error\")"
    "(error 'simple-error :format-control \"custom\")"

    ;; Custom conditions
    "(signal 'condition)")))

(defun generate-warning-code ()
  "Generate code that signals warnings"
  (random-choice
   (list
    "(warn \"test warning\")"
    "(warn \"level ~A\" (random 10))"
    "(progn (warn \"first\") (warn \"second\") :ok)")))

(defun expected-condition-type (code)
  "Return expected condition type for error code"
  (cond
    ((search "/ 1 0" code) "DIVISION-BY-ZERO")
    ((search "car 42" code) "TYPE-ERROR")
    ((search "undefined-variable" code) "UNBOUND-VARIABLE")
    ((search "undefined-function" code) "UNDEFINED-FUNCTION")
    ((search "error" code) "ERROR")
    (t "CONDITION")))
```

### Property 1: Errors Don't Crash Server

```lisp
(fiveam:test errors-dont-crash-server
  "No error crashes the server"
  (let ((session (make-test-session)))
    (dotimes (trial 100)
      (let* ((error-code (generate-error-code))
             (r1 (evaluate-in-session session error-code)))

        ;; Should get error response, not crash
        (fiveam:is (result-field r1 :isError))

        ;; Server should still be operational
        (let ((r2 (evaluate-in-session session "(+ 1 2)")))
          (fiveam:is (not (result-field r2 :isError)))
          (fiveam:is (search "3" (result-value r2))))))))
```

### Property 2: Condition Type Preservation

```lisp
(fiveam:test condition-type-preservation
  "Error responses include specific condition type"
  (let ((session (make-test-session))
        (test-cases '(("(/ 1 0)" . "DIVISION-BY-ZERO")
                     ("(car 42)" . "TYPE-ERROR")
                     ("undefined-var" . "UNBOUND-VARIABLE")
                     ("(undefined-func)" . "UNDEFINED-FUNCTION")
                     ("(error \"test\")" . "ERROR"))))

    (dolist (test test-cases)
      (destructuring-bind (code . expected-type) test
        (let* ((result (evaluate-in-session session code))
               (text (result-content-text result)))
          (fiveam:is (result-field result :isError))
          (fiveam:is (search expected-type text)
                    "Expected ~A in error for: ~A" expected-type code))))))
```

### Property 3: Warning Capture Without Stopping

```lisp
(fiveam:test warnings-capture-continue
  "Warnings are captured but evaluation continues"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((warning-code (generate-warning-code))
             (result (evaluate-in-session session warning-code))
             (text (result-content-text result)))

        ;; Should succeed (warnings don't stop evaluation)
        (fiveam:is (not (result-field result :isError)))

        ;; Warning should be captured
        (fiveam:is (or (search "[warnings]" text)
                      (search "WARNING" text)))

        ;; Should have return value
        (fiveam:is (or (search "=>" text)
                      (search "NIL" text)))))))
```

### Property 4: Backtrace Included in Errors

```lisp
(fiveam:test backtrace-in-errors
  "Error responses include backtrace"
  (let ((session (make-test-session)))
    (dotimes (trial 20)
      (let* ((error-code (generate-error-code))
             (result (evaluate-in-session session error-code))
             (text (result-content-text result)))

        (when (result-field result :isError)
          ;; Should have backtrace section
          (fiveam:is (search "[Backtrace]" text)
                    "Missing backtrace for: ~A" error-code))))))
```

### Property 5: Output Captured Before Error

```lisp
(fiveam:test output-before-error
  "Output produced before error is captured"
  (let ((session (make-test-session))
        (test-cases '("(princ \"hello\") (error \"boom\")"
                     "(format t \"info~%\") (/ 1 0)"
                     "(print 123) undefined-var")))

    (dolist (code test-cases)
      (let* ((result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Should be error
        (fiveam:is (result-field result :isError))

        ;; Should have captured output
        (fiveam:is (or (search "[stdout]" text)
                      (and (search "hello" text)
                           (search "info" text)
                           (search "123" text))))))))
```

### Property 6: Multiple Warnings Captured

```lisp
(fiveam:test multiple-warnings-captured
  "All warnings in an evaluation are captured"
  (let ((session (make-test-session)))
    (dotimes (trial 20)
      (let* ((count (+ 2 (random 5)))
             (code (format nil "~{(warn \"w~A\")~^ ~} :done"
                          (loop for i from 1 to count collect i)))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Should succeed
        (fiveam:is (not (result-field result :isError)))

        ;; Should have warnings section
        (fiveam:is (search "[warnings]" text))

        ;; Should capture all warnings
        (fiveam:is (>= (count-occurrences "WARNING" text) count))))))
```

### Property 7: Nested Error Handling

```lisp
(fiveam:test nested-error-handling
  "Errors in error handlers are caught"
  (let ((session (make-test-session)))
    ;; This is tricky - define an error handler that itself errors
    (let* ((code "(handler-case (error \"outer\") (error (c) (error \"inner\")))")
           (result (evaluate-in-session session code)))

      ;; Should get error response (not hang or crash)
      (fiveam:is (result-field result :isError))

      ;; Server should still work
      (let ((r2 (evaluate-in-session session "(+ 1 1)")))
        (fiveam:is (not (result-field r2 :isError)))))))
```

### Property 8: Server Operational After Many Errors

```lisp
(fiveam:test server-survives-many-errors
  "Server remains operational after many consecutive errors"
  (let ((session (make-test-session)))
    ;; Generate 50 different errors
    (dotimes (i 50)
      (let* ((error-code (generate-error-code))
             (result (evaluate-in-session session error-code)))
        (fiveam:is (result-field result :isError))))

    ;; Server should still work perfectly
    (let ((result (evaluate-in-session session "(list 1 2 3)")))
      (fiveam:is (not (result-field result :isError)))
      (fiveam:is (search "(1 2 3)" (result-value result))))))
```

## Configuration

- Trials: 100 for normal runs
- Trials: 500 for thorough testing
- Shrinking: Enabled

## Helper Functions

```lisp
(defun count-occurrences (substring string)
  "Count how many times substring appears in string"
  (loop with start = 0
        for pos = (search substring string :start2 start)
        while pos
        count pos
        do (setf start (1+ pos))))
```

## Assertions

For every trial:
1. Errors produce error responses, not crashes
2. Error responses include specific condition type
3. Warnings are captured but don't stop evaluation
4. Error responses include backtrace
5. Output before error is preserved
6. Multiple warnings are all captured
7. Server remains operational after errors

## Notes

- This test verifies INV-002 (Server Stability) and INV-006 (Condition Type Preservation)
- Tests diverse error types: arithmetic, type, undefined, custom
- Verifies warnings vs errors are handled differently
- Confirms server resilience to many errors
