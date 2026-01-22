# How to Handle Errors Gracefully

**Problem**: You need to write code that handles errors without crashing or producing confusing output.

Common Lisp's condition system is powerful. This guide shows you how to handle errors gracefully within CL-MCP-Server evaluations.

## Prerequisites

- CL-MCP-Server is running and connected to Claude
- Basic understanding of Common Lisp conditions

## Understanding Errors in CL-MCP-Server

When an error occurs:

1. The condition is caught by the server
2. Error type, message, and backtrace are captured
3. Session state is preserved (no corruption)
4. Server remains operational for next evaluation

**Example**:
```
User: Please evaluate (/ 1 0)
Claude: [ERROR] DIVISION-BY-ZERO
        Arithmetic error DIVISION-BY-ZERO signalled.
        Operation was (/ 1 0).

        [Backtrace]
        0: (/ 1 0)
```

## Solution 1: handler-case (Catch and Recover)

Use `handler-case` to catch specific error types:

```lisp
(handler-case
    (/ 1 0)  ; Code that might error
  (division-by-zero (c)
    (format nil "Cannot divide by zero: ~A" c)
    :error-handled))

;; => :ERROR-HANDLED
```

**Example with multiple handlers**:

```lisp
(defun safe-divide (x y)
  (handler-case
      (/ x y)
    (division-by-zero ()
      (format t "Error: Division by zero~%")
      nil)
    (type-error (c)
      (format t "Error: Wrong type - ~A~%" c)
      nil)
    (error (c)
      (format t "Unexpected error: ~A~%" c)
      nil)))

(safe-divide 10 0)   ;; => NIL (with error message printed)
(safe-divide 10 "x") ;; => NIL (with type error message)
(safe-divide 10 2)   ;; => 5
```

## Solution 2: ignore-errors (Simple Error Suppression)

For simple cases where you just want to suppress errors:

```lisp
(ignore-errors
  (/ 1 0))
;; => NIL, #<DIVISION-BY-ZERO ...>
;; Returns two values: result (nil on error) and the condition
```

**Check if an error occurred**:

```lisp
(multiple-value-bind (result condition)
    (ignore-errors
      (/ 1 0))
  (if condition
      (format t "Error occurred: ~A~%" condition)
      (format t "Result: ~A~%" result)))
```

## Solution 3: handler-bind (Handle Without Unwinding)

Use `handler-bind` when you need to handle the error but continue execution:

```lisp
(handler-bind
    ((warning #'muffle-warning))  ; Suppress warnings
  (defun foo ()
    (declare (ignore))
    42))

;; Warnings are muffled, function is defined
```

**Logging errors without stopping**:

```lisp
(defvar *error-log* nil)

(defun logged-computation ()
  (handler-bind
      ((error (lambda (c)
                (push (format nil "Error at ~A: ~A"
                              (get-universal-time) c)
                      *error-log*))))
    (+ 1 2)
    (/ 1 0)  ; This will log but also error
    (+ 3 4)))

;; The error is logged in *error-log* before being raised
```

## Common Error Types

### Undefined Function

```lisp
(handler-case
    (nonexistent-function 1 2 3)
  (undefined-function (c)
    (format t "Function ~A is not defined~%"
            (cell-error-name c))
    :undefined))
```

### Unbound Variable

```lisp
(handler-case
    (+ undefined-var 1)
  (unbound-variable (c)
    (format t "Variable ~A is not bound~%"
            (cell-error-name c))
    :unbound))
```

### Type Errors

```lisp
(handler-case
    (+ 1 "hello")
  (type-error (c)
    (format t "Expected ~A but got ~A~%"
            (type-error-expected-type c)
            (type-error-datum c))
    :type-error))
```

### Index Out of Bounds

```lisp
(handler-case
    (aref #(1 2 3) 10)
  (error (c)
    (format t "Array access error: ~A~%" c)
    :bounds-error))
```

## Defensive Programming Patterns

### Validate Before Operating

```lisp
(defun safe-nth (n list)
  "Safely get nth element, returning nil if out of bounds"
  (when (and (>= n 0) (< n (length list)))
    (nth n list)))

(safe-nth 10 '(a b c))  ;; => NIL (no error)
```

### Use Type Checks

```lisp
(defun typed-divide (x y)
  "Divide with type checking"
  (check-type x number)
  (check-type y number)
  (when (zerop y)
    (error "Division by zero: ~A / ~A" x y))
  (/ x y))

(typed-divide 10 2)   ;; => 5
(typed-divide 10 "x") ;; => [ERROR] TYPE-ERROR with clear message
```

### Provide Restarts

Advanced: offer ways to recover from errors:

```lisp
(defun divide-with-restart (x y)
  (restart-case
      (if (zerop y)
          (error "Division by zero")
          (/ x y))
    (return-zero ()
      :report "Return zero instead"
      0)
    (return-nil ()
      :report "Return nil instead"
      nil)
    (use-value (value)
      :report "Specify a value to use as divisor"
      :interactive (lambda () (list (read)))
      (/ x value))))

;; In interactive contexts, restarts can be invoked
;; In MCP context, they're part of the backtrace info
```

## Assertions

Use `assert` for invariants that should never be violated:

```lisp
(defun process-positive (n)
  (assert (> n 0) (n)
          "Expected positive number, got ~A" n)
  (* n 2))

(process-positive 5)   ;; => 10
(process-positive -1)  ;; => [ERROR] SIMPLE-ERROR with message
```

## Warnings vs Errors

### Handling Warnings

Warnings don't stop execution but are reported:

```lisp
(defun careful-operation ()
  (warn "This operation is deprecated")
  (+ 1 2))

;; Evaluation continues, warning is reported separately
```

### Muffling Warnings

```lisp
(handler-bind ((warning #'muffle-warning))
  (defun foo ()
    (declare (ignore))  ; Would normally warn
    42))
```

## Best Practices

### 1. Be Specific with Error Handlers

```lisp
;; Good - specific handlers
(handler-case (operation)
  (division-by-zero () :div-zero)
  (type-error () :type-error)
  (file-error () :file-error))

;; Avoid - too broad
(handler-case (operation)
  (error () :any-error))  ; Catches everything
```

### 2. Always Preserve Error Information

```lisp
;; Good - log the condition
(handler-case (operation)
  (error (c)
    (format t "Error details: ~A~%" c)
    :failed))

;; Avoid - silently swallow errors
(handler-case (operation)
  (error () :failed))  ; Loses information
```

### 3. Clean Up Resources

```lisp
(unwind-protect
    (progn
      (open-resource)
      (risky-operation))
  (close-resource))  ; Always runs, even on error
```

## Common Errors and Fixes

### Error: Server Stops Responding

**This won't happen**. The server catches all errors from user code and continues operating.

### Error: State Corrupted After Error

**This won't happen**. Session state is preserved even when errors occur. Subsequent evaluations work normally.

### Can't Recover from Error

If you need to continue after an error, use `handler-case` or `ignore-errors` as shown above.

## Verification

Test your error handling:

```lisp
;; Test that error is caught
(defun test-error-handling ()
  (let ((result
          (handler-case
              (/ 1 0)
            (division-by-zero () :caught))))
    (assert (eq result :caught))
    :test-passed))

(test-error-handling)  ;; => :TEST-PASSED
```

## See Also

- [Evaluation Error Scenarios](../../canon/features/error-handling/scenarios/evaluation-errors.md) - All error types handled by server
- [Condition Report Contract](../../canon/features/error-handling/contracts/condition-report.md) - How errors are formatted
- [CLtL2: Conditions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html) - Complete condition system reference
- [Practical Common Lisp: Beyond Exception Handling](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html) - Excellent tutorial
