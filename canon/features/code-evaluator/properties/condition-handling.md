---
type: property
name: condition-handling
version: 1.0.0
feature: code-evaluator
covers:
  - contracts/evaluate-lisp-tool
relates_to:
  - core/invariants#INV-002
  - core/invariants#INV-006
---

# Condition Handling Property

## Statement

**For all** code evaluations,
**if** the code signals a condition (error, warning, or serious-condition),
**then**:
1. The server MUST NOT crash or terminate
2. Errors MUST be captured and returned as structured error responses
3. Warnings MUST be captured and included in the response (but evaluation continues)
4. The condition type MUST be preserved and reported
5. The server MUST remain operational for subsequent requests

## Formal Expression

```
∀ code ∈ Evaluations :
  let result = evaluate(code) in
    (signals_error(code) ⟹
      result.isError = true ∧
      result.error_type = condition_type(code) ∧
      result.has_backtrace = true ∧
      server_is_operational())
    ∧
    (signals_warning(code) ⟹
      result.isError = false ∧
      result.warnings ≠ [] ∧
      has_return_value(result))

where condition_type ∈ {
  ERROR, TYPE-ERROR, UNBOUND-VARIABLE, UNDEFINED-FUNCTION,
  DIVISION-BY-ZERO, FILE-ERROR, PARSE-ERROR, ...
}
```

## Informal Explanation

User code may signal various conditions. The evaluator must handle them gracefully:

### Error Handling

When code signals an error:
```lisp
(/ 1 0) → DIVISION-BY-ZERO error
```

Response includes:
- `isError: true`
- Condition type: `DIVISION-BY-ZERO`
- Condition message: "arithmetic error DIVISION-BY-ZERO signalled"
- Backtrace showing where error occurred
- Any output produced before error

Server continues running after error.

### Warning Handling

When code signals a warning:
```lisp
(warn "This is a warning") :ok
```

Response includes:
- `isError: false` (warnings don't stop evaluation)
- `[warnings]` section with warning messages
- Return value: `:OK`
- Evaluation continues after warning

### Condition Types Handled

| Condition | Behavior |
|-----------|----------|
| `error` | Catch, format, return as error response |
| `type-error` | Catch, preserve type, return as error |
| `unbound-variable` | Catch, show variable name |
| `undefined-function` | Catch, show function name |
| `warning` | Record and muffle, continue evaluation |
| `style-warning` | Record and muffle, continue evaluation |
| `serious-condition` | Catch, format, return as error |
| `storage-condition` | Catch, format, return as error |

### No Condition Leaks

User code conditions must never propagate up to:
- MCP protocol handler
- JSON-RPC handler
- Server main loop

All conditions are caught at evaluation boundary.

## Rationale

Without proper condition handling:
- `(/ 1 0)` would crash the server
- Session state would be lost
- Claude loses REPL context
- Must restart entire connection

With proper handling:
- Errors are reported as structured responses
- Session state remains intact
- User can fix code and retry
- No interruption to workflow

This property directly implements:
- INV-002 (Server Stability): Server never terminates due to evaluation errors
- INV-006 (Condition Type Preservation): Condition types are reported

## Counterexample Shape

If this property is violated, you might see:
- Server crashes on `(error "boom")`
- Error response missing condition type (just message)
- Warning stops evaluation instead of being captured
- Subsequent requests fail after error
- Backtrace missing from error response
- Generic "ERROR" instead of specific type like "TYPE-ERROR"
- Server hangs on certain conditions

## Verification Approach

**Property Test - Error Doesn't Crash Server**:

```lisp
(defun test-error-doesnt-crash ()
  (let ((session (make-test-session)))
    ;; Trigger error
    (let ((r1 (evaluate session "(error \"boom\")")))
      (assert (not (result-success-p r1)))
      (assert (search "ERROR" (result-error r1))))

    ;; Server should still work
    (let ((r2 (evaluate session "(+ 1 2)")))
      (assert (result-success-p r2)))))
```

**Property Test - Condition Type Preservation**:

```lisp
(defun test-condition-type-preservation ()
  (let* ((test-cases
          '(("(car 42)"              . "TYPE-ERROR")
            ("(/ 1 0)"               . "DIVISION-BY-ZERO")
            ("undefined-var"         . "UNBOUND-VARIABLE")
            ("(undefined-func)"      . "UNDEFINED-FUNCTION")
            ("(error \"custom\")"    . "SIMPLE-ERROR")))
         (session (make-test-session)))
    (dolist (test test-cases)
      (let* ((code (car test))
             (expected-type (cdr test))
             (result (evaluate session code)))
        (assert (not (result-success-p result)))
        (assert (search expected-type (result-error result)))))))
```

**Property Test - Warning Capture**:

```lisp
(defun test-warning-capture ()
  (let ((result (evaluate "(warn \"caution\") :ok")))
    ;; Warnings don't stop evaluation
    (assert (result-success-p result))
    (assert (equal ":OK" (result-value result)))

    ;; But warning is captured
    (assert (= 1 (length (result-warnings result))))
    (assert (search "caution" (first (result-warnings result))))))
```

**Property Test - Multiple Warnings**:

```lisp
(defun test-multiple-warnings ()
  (let ((result (evaluate "(warn \"w1\") (warn \"w2\") (warn \"w3\") :done")))
    (assert (result-success-p result))
    (assert (= 3 (length (result-warnings result))))))
```

**Property Test - Backtrace Included**:

```lisp
(defun test-backtrace-included ()
  (let ((result (evaluate "(defun f () (error \"fail\")) (f)")))
    (assert (not (result-success-p result)))
    (assert (search "[Backtrace]" (result-error result)))
    (assert (search "F" (result-error result)))))  ; Function name in trace
```

**Property Test - Output Before Error**:

```lisp
(defun test-output-before-error ()
  (let ((result (evaluate "(format t \"hello\") (error \"boom\")")))
    (assert (not (result-success-p result)))
    (assert (equal "hello" (result-stdout result)))))
```

**Fuzz Test - Random Errors**:

```lisp
(defun test-random-errors ()
  (let ((session (make-test-session))
        (error-forms
         '("(error \"test\")"
           "(/ 1 0)"
           "(car 'not-a-cons)"
           "(funcall 'not-a-function)"
           "undefined-symbol"
           "(1+ \"string\")")))
    ;; None should crash server
    (dolist (form error-forms)
      (let ((result (evaluate session form)))
        (assert (not (result-success-p result)))))

    ;; Server should still work
    (let ((result (evaluate session "(+ 1 2)")))
      (assert (result-success-p result)))))
```

**Generator**: Generate random code that signals conditions:
- Various error types (type-error, arithmetic-error, etc.)
- Warning calls
- Undefined references
- Type mismatches

**Assertion**:
- No condition crashes server
- All errors have condition type in response
- All warnings are captured
- Server operational after every condition

**Shrinking**: Find minimal code that violates condition handling

**Edge Cases**:
- Nested errors (error handler that errors)
- Very long error messages
- Errors during class initialization
- Errors in macro expansion
- Stack overflow from infinite recursion

## Related

- [evaluation-errors scenario](../../error-handling/scenarios/evaluation-errors.md)
- [INV-002: Server Stability](../../../core/foundation/invariants.md#inv-002)
- [INV-006: Condition Type Preservation](../../../core/foundation/invariants.md#inv-006)
