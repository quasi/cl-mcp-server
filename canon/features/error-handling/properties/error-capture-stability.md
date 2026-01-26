---
type: property
name: error-capture-stability
version: 1.0.0
feature: error-handling
covers:
  - contracts/condition-report
relates_to:
  - core/invariants#INV-002
---

# Error Capture Stability Property

## Statement

**For all** code that signals errors during evaluation,
**the server** MUST capture the error and return a structured error response
AND **the server** MUST remain operational for subsequent evaluations.

## Formal Expression

```
∀ code ∈ ErrorProducingCode :
  let response = evaluate(code) in
    response.isError = true ∧
    is_valid_error_response(response) ∧
    server_remains_operational()

where server_remains_operational() ≡
  ∀ subsequent_code : can_evaluate(subsequent_code)
```

## Informal Explanation

When user code signals any condition (error, serious-condition, storage-condition), the server must:

1. **Capture it**: Use `handler-bind` to intercept the condition without unwinding
2. **Report it**: Format as structured error response per condition-report contract
3. **Survive it**: Continue accepting and processing new evaluation requests

This applies to:
- All standard Common Lisp error conditions
- Implementation-specific errors
- User-defined condition types
- Serious conditions (e.g., storage exhaustion)
- Nested errors (errors during error handling)

The server process must never terminate due to evaluation errors.

## Rationale

User code is untrusted and may contain bugs. Crashing the server on evaluation errors would:
- Lose all session state (definitions, loaded systems)
- Interrupt Claude's workflow
- Require reinitialization (expensive)
- Frustrate users with fragile behavior

By catching all conditions and reporting them gracefully, the server provides a robust REPL experience where errors are normal, expected, and recoverable.

## Counterexample Shape

If this property is violated, you might see:

**Server Crash**:
```
$ sbcl --eval "(cl-mcp-server:start)"
...
User evaluates: (error "boom")
[Server process terminates with unhandled error]
```

**Propagated Error** (server returns malformed response):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32603,
    "message": "Internal error: unhandled SIMPLE-ERROR"
  }
}
```
This is wrong because the error should be in the tool result, not a JSON-RPC protocol error.

**Correct Behavior**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{"type": "text", "text": "[ERROR] SIMPLE-ERROR\nboom\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

Other violations:
- Server becomes unresponsive after evaluation error
- Server enters invalid state where subsequent evaluations fail
- Server loses session state after error
- Server crashes on specific condition types

## Verification Approach

**Stress Testing**: Generate diverse error-producing code

```lisp
(defvar *error-generators*
  '(;; Standard errors
    "(error \"boom\")"
    "(cerror \"continue\" \"deliberate error\")"

    ;; Type errors
    "(+ 1 \"string\")"
    "(car 'not-a-list)"

    ;; Undefined symbols
    "(nonexistent-function)"
    "(print undefined-variable)"

    ;; Arithmetic errors
    "(/ 1 0)"
    "(ash 1 999999999)"  ; may signal storage condition

    ;; Array bounds
    "(aref #(1 2 3) 100)"

    ;; Reader errors
    "(read-from-string \"(+ 1 2\")"

    ;; Package errors
    "(find-symbol \"X\" \"NONEXISTENT-PACKAGE\")"

    ;; File errors
    "(open \"/nonexistent/path.lisp\")"

    ;; Serious conditions
    "(sb-ext:run-program \"/nonexistent\" '())"  ; SBCL specific

    ;; Recursive errors
    "(handler-bind ((error (lambda (c) (error \"nested\"))))"
    "  (error \"outer\"))"))
```

**Assertion Protocol**:
```lisp
(defun verify-error-capture-stability (error-code)
  ;; Phase 1: Verify error is captured
  (let ((response (evaluate-lisp error-code)))
    (assert (error-response-p response)
            nil
            "Error code did not produce error response: ~A" error-code)

    (assert (valid-error-format-p response)
            nil
            "Error response malformed for: ~A" error-code)

    (assert (contains-condition-type-p response)
            nil
            "Error response missing condition type for: ~A" error-code))

  ;; Phase 2: Verify server still works
  (let ((test-response (evaluate-lisp "(+ 1 1)")))
    (assert (result-response-p test-response)
            nil
            "Server broken after error: ~A" error-code)

    (assert (string= (response-value test-response) "2")
            nil
            "Server computation broken after error: ~A" error-code))

  ;; Phase 3: Verify session state preserved
  (evaluate-lisp "(defvar *test-var* 42)")
  (let ((response (evaluate-lisp error-code)))
    (assert (error-response-p response)))

  (let ((check-response (evaluate-lisp "*test-var*")))
    (assert (result-response-p check-response)
            nil
            "Session state lost after error: ~A" error-code)

    (assert (string= (response-value check-response) "42")
            nil
            "Session state corrupted after error: ~A" error-code)))
```

**Property Test**:
1. For each error generator:
   - Send evaluation request with error-producing code
   - Verify response has `isError: true`
   - Verify response format matches contract
   - Send follow-up request `(+ 1 1)`
   - Verify server responds correctly
2. Run 1000 iterations with random error code
3. Verify server never crashes or becomes unresponsive
4. Verify session state persists across errors

**Concurrency Test** (if applicable):
- Send multiple error-producing evaluations in rapid succession
- Verify all receive error responses
- Verify server handles concurrent errors without deadlock

**Edge Cases**:
- Errors during initialization (before session ready)
- Errors in condition formatting code (recursive error handling)
- Stack overflow errors
- Memory exhaustion (storage-condition)
- Interrupts (if supported)

**Shrinking**: Find minimal error-producing code that crashes server or leaves it inoperable
