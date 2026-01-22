# The Lisp Condition System

**Understanding Common Lisp's powerful error handling mechanism and how CL-MCP-Server leverages it.**

Common Lisp's condition system is more sophisticated than simple exception handling found in most languages. This document explains what makes it special and how CL-MCP-Server captures and reports conditions.

## What Are Conditions?

In Common Lisp, a **condition** is any event that might require special handling:

- **Errors**: Problems that prevent normal execution
- **Warnings**: Issues that don't stop execution but deserve attention
- **Information**: General notifications

```lisp
;; Error condition
(/ 1 0)
;; => Signals DIVISION-BY-ZERO

;; Warning condition
(defun foo (x)
  (declare (ignore))  ; Parameter declared but not actually ignored
  42)
;; => Warning: The variable X is defined but never used

;; Custom condition
(signal 'my-custom-condition :data "some info")
```

## Conditions vs Exceptions

Most languages use exceptions:

```python
# Python - exception model
try:
    result = risky_operation()
except SpecificError:
    result = handle_error()
finally:
    cleanup()
```

**Limitation**: Once an exception is raised, the stack unwinds immediately. The error-handling code is far removed from where the error occurred.

Common Lisp uses conditions **with restarts**:

```lisp
;; Lisp - condition model with restarts
(restart-case
    (risky-operation)
  (use-value (value)
    :report "Provide a value to use instead"
    value)
  (skip-operation ()
    :report "Skip this operation"
    nil))
```

**Power**: Error-handling code can communicate with the code that signaled the error, offering multiple recovery strategies **without unwinding the stack**.

## Key Concepts

### 1. Signaling Conditions

Conditions are signaled (not "thrown"):

```lisp
;; Signal an error (stops execution)
(error "Something went wrong: ~A" detail)

;; Signal a warning (continues execution)
(warn "This is deprecated")

;; Signal any condition
(signal 'my-condition)
```

### 2. Handling Conditions

**handler-case** (like try/catch):

```lisp
(handler-case
    (potentially-failing-operation)
  (division-by-zero ()
    (format t "Can't divide by zero")
    0)
  (type-error (c)
    (format t "Wrong type: ~A" c)
    nil))
```

**handler-bind** (handlers run before unwinding):

```lisp
(handler-bind
    ((warning #'muffle-warning))  ; Suppress warnings
  (code-that-might-warn))
```

### 3. Restarts

Restarts are recovery options established by the code that might signal:

```lisp
(defun divide-with-restarts (x y)
  (restart-case
      (if (zerop y)
          (error 'division-by-zero)
          (/ x y))
    (return-zero ()
      :report "Return zero"
      0)
    (return-nil ()
      :report "Return nil"
      nil)
    (use-value (value)
      :report "Specify a divisor to use"
      :interactive (lambda () (list (read)))
      (/ x value))))
```

**Invocation**: A handler higher up the stack can invoke a restart:

```lisp
(handler-bind
    ((division-by-zero
       (lambda (c)
         (declare (ignore c))
         (invoke-restart 'return-zero))))
  (divide-with-restarts 10 0))
;; => 0 (restart was invoked)
```

### 4. Condition Hierarchy

Conditions form a type hierarchy:

```
condition
├── warning
│   ├── style-warning
│   └── simple-warning
└── serious-condition
    └── error
        ├── simple-error
        ├── type-error
        ├── arithmetic-error
        │   ├── division-by-zero
        │   └── floating-point-overflow
        ├── control-error
        ├── file-error
        ├── package-error
        ├── stream-error
        ├── cell-error
        │   ├── unbound-variable
        │   └── undefined-function
        └── ...
```

**Handlers can be specific or general**:

```lisp
(handler-case (operation)
  (division-by-zero () :div-zero)     ; Specific
  (arithmetic-error () :arithmetic)   ; More general
  (error () :any-error))              ; Catch-all
```

## How CL-MCP-Server Handles Conditions

### Capturing All Conditions

The server wraps all user code in a comprehensive handler:

```lisp
(handler-case
    (evaluate-user-code)
  (error (c)
    (report-condition c :is-error t))
  (warning (c)
    (report-condition c :is-error nil)))
```

**Result**: No matter what condition user code signals, the server catches it and formats a response.

### Structured Error Reporting

CL-MCP-Server captures rich information:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{
      "type": "text",
      "text": "[ERROR] DIVISION-BY-ZERO\nArithmetic error DIVISION-BY-ZERO signalled.\nOperation was (/ 1 0).\n\n[Backtrace]\n0: (/ 1 0)\n1: (EVAL (/ 1 0))\n..."
    }],
    "isError": true
  }
}
```

**What's captured**:
- **Condition Type**: `DIVISION-BY-ZERO` (not just "Error")
- **Message**: Human-readable description
- **Backtrace**: Stack trace at the point of error
- **Error Flag**: `isError: true` for MCP protocol

### Condition Type Preservation

The server preserves the exact condition type:

```lisp
(/ 1 0)           ; => [ERROR] DIVISION-BY-ZERO
(+ 1 "hello")     ; => [ERROR] TYPE-ERROR
(undefined-func)  ; => [ERROR] UNDEFINED-FUNCTION
(aref #() 10)     ; => [ERROR] SB-KERNEL:INDEX-TOO-LARGE-ERROR
```

**Why it matters**: Specific error types help diagnose problems. "Something went wrong" is less useful than "Division by zero in calculate-average at line 42."

### Warning Handling

Warnings don't stop execution:

```lisp
(defun compute (x)
  (warn "Using deprecated function")
  (* x 2))

(compute 5)
```

**Output**:
```
[WARNING] SIMPLE-WARNING
Using deprecated function

=> 10
```

Both the warning **and** the result are returned.

### Backtrace Capture

The server captures the call stack:

```lisp
(defun inner (x)
  (/ 100 x))

(defun outer (x)
  (inner x))

(outer 0)
```

**Output**:
```
[ERROR] DIVISION-BY-ZERO
Arithmetic error DIVISION-BY-ZERO signalled.

[Backtrace]
0: (/ 100 0)
1: (INNER 0)
2: (OUTER 0)
3: (EVAL (OUTER 0))
...
```

You can see the entire call chain that led to the error.

## Common Error Types in CL-MCP-Server

### Evaluation Errors

**UNDEFINED-FUNCTION**: Calling a function that doesn't exist

```lisp
(nonexistent-function 1 2 3)
;; => [ERROR] UNDEFINED-FUNCTION
;;    The function NONEXISTENT-FUNCTION is undefined.
```

**UNBOUND-VARIABLE**: Referencing a variable that's not bound

```lisp
(+ undefined-var 1)
;; => [ERROR] UNBOUND-VARIABLE
;;    The variable UNDEFINED-VAR is unbound.
```

**TYPE-ERROR**: Wrong type for an operation

```lisp
(+ 1 "hello")
;; => [ERROR] TYPE-ERROR
;;    The value "hello" is not of type NUMBER.
```

### Arithmetic Errors

**DIVISION-BY-ZERO**: Division by zero

```lisp
(/ 1 0)
;; => [ERROR] DIVISION-BY-ZERO
;;    Arithmetic error DIVISION-BY-ZERO signalled.
```

**FLOATING-POINT-OVERFLOW**: Numeric overflow

```lisp
(exp 1000.0)
;; => [ERROR] FLOATING-POINT-OVERFLOW
;;    Floating-point overflow occurred.
```

### Reader Errors

**END-OF-FILE**: Incomplete expression

```lisp
(+ 1 2
;; => [ERROR] END-OF-FILE
;;    Unexpected end of file while reading.
```

**READER-ERROR**: Invalid syntax

```lisp
(eval (read-from-string "#<invalid>"))
;; => [ERROR] READER-ERROR
;;    Unreadable object syntax.
```

### Package Errors

**PACKAGE-ERROR**: Package not found

```lisp
(in-package :nonexistent)
;; => [ERROR] PACKAGE-ERROR
;;    The name "NONEXISTENT" does not designate any package.
```

### Index Errors

**INDEX-TOO-LARGE-ERROR**: Array/sequence index out of bounds

```lisp
(aref #(1 2 3) 10)
;; => [ERROR] SB-KERNEL:INDEX-TOO-LARGE-ERROR
;;    Invalid index 10 for (SIMPLE-VECTOR 3).
```

## Server Stability Guarantees

### Invariant: Server Never Crashes

**INV-002**: Server never terminates due to evaluation errors.

**Implementation**: Top-level handler catches all conditions:

```lisp
(handler-case
    (process-request request)
  (error (c)
    (log-error c)
    (send-error-response c)))
```

No matter what user code does, the server remains operational.

### Session State Preservation

Errors don't corrupt session state:

```lisp
;; Define a function
(defun safe-function () 42)

;; Trigger an error
(/ 1 0)
;; => [ERROR] DIVISION-BY-ZERO

;; Previous definitions still work
(safe-function)
;; => 42
```

**Postcondition**: After an error, the session is in the same state as before the failed evaluation (except for any side effects that occurred before the error).

## Advanced: Custom Conditions

You can define custom condition types:

```lisp
;; Define a custom condition
(define-condition invalid-configuration (error)
  ((field :initarg :field :reader config-field)
   (value :initarg :value :reader config-value))
  (:report (lambda (condition stream)
             (format stream "Invalid configuration: ~A = ~A"
                     (config-field condition)
                     (config-value condition)))))

;; Signal it
(error 'invalid-configuration
       :field "port"
       :value "not-a-number")
;; => [ERROR] INVALID-CONFIGURATION
;;    Invalid configuration: port = not-a-number
```

## Comparison with Other Languages

### Python

```python
try:
    risky_operation()
except SpecificError as e:
    handle(e)
finally:
    cleanup()
```

**Similarities**: Both have try/except-like constructs (`handler-case`)

**Differences**:
- Python unwinds immediately; Lisp can keep the stack
- Python doesn't have restarts
- Lisp has separate warning/error/info condition types

### Java

```java
try {
    riskyOperation();
} catch (SpecificException e) {
    handle(e);
} finally {
    cleanup();
}
```

**Similarities**: Exception hierarchy, try/catch/finally

**Differences**:
- Java checked exceptions (compile-time); Lisp conditions (runtime)
- Java no restarts
- Lisp more flexible handler attachment

### JavaScript

```javascript
try {
    riskyOperation();
} catch (e) {
    handle(e);
} finally {
    cleanup();
}
```

**Similarities**: Basic error catching

**Differences**:
- JavaScript minimal error types; Lisp rich hierarchy
- JavaScript no warnings separate from errors
- Lisp restarts enable sophisticated recovery

### Go

```go
result, err := riskyOperation()
if err != nil {
    handle(err)
}
```

**Similarities**: Explicit error handling

**Differences**:
- Go error values; Lisp condition objects with inheritance
- Go no stack unwinding; Lisp has unwinding + restarts
- Go manual propagation; Lisp automatic signaling

## Best Practices

### For Users of CL-MCP-Server

1. **Read the condition type**: Don't just look at the message—the type tells you what went wrong

2. **Check the backtrace**: It shows the call chain that led to the error

3. **Understand the error hierarchy**: A handler for `arithmetic-error` catches `division-by-zero`

4. **Use specific handlers**: Catch specific conditions when you know how to handle them

### For Contributing Developers

1. **Signal appropriate types**: Use specific condition types, not generic `error`

2. **Provide restarts when appropriate**: Give callers recovery options

3. **Include useful reports**: Override `:report` to give helpful messages

4. **Preserve condition information**: Don't swallow conditions without logging

## See Also

- [Error Handling Contract](../../canon/features/error-handling/contracts/condition-report.md) - How conditions are formatted
- [Evaluation Error Scenarios](../../canon/features/error-handling/scenarios/evaluation-errors.md) - All error types the server handles
- [How to Handle Errors Gracefully](../how-to/handle-errors-gracefully.md) - Practical error-handling patterns
- [Practical Common Lisp: Beyond Exception Handling](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html) - Comprehensive tutorial on conditions
- [CLtL2: Conditions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html) - Complete specification
