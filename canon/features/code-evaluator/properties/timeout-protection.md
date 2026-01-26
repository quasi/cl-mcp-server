---
type: property
name: timeout-protection
version: 1.0.0
feature: code-evaluator
covers:
  - contracts/evaluate-lisp-tool
  - contracts/configure-limits-tool
relates_to:
  - core/invariants#INV-002
---

# Timeout Protection Property

## Statement

**For all** code evaluations,
**if** execution time exceeds the configured timeout,
**then** the evaluation MUST be terminated with a timeout error
AND **the server** MUST remain operational for subsequent requests.

## Formal Expression

```
∀ code ∈ Evaluations, ∀ timeout ∈ ℕ :
  let start = current_time() in
  let result = evaluate(code, timeout) in
  let duration = current_time() - start in
    (duration > timeout ⟹
      result.isError = true ∧
      result.error_type = "TIMEOUT" ∧
      server_is_operational())
```

## Informal Explanation

Infinite loops and expensive computations must not hang the server:

1. **Timeout enforcement**: Code taking longer than configured limit is interrupted
2. **Graceful termination**: Timeout produces structured error response (not crash)
3. **Server stability**: Server remains operational after timeout
4. **Partial output capture**: Any output produced before timeout is preserved
5. **Configurable limits**: Users can adjust timeout via `configure-limits` tool

Default timeout: 30 seconds
Timeout can be disabled (set to 0), but this is not recommended.

Timeout error includes:
- Clear indication this was a timeout
- The duration that was exceeded
- Hint about infinite loops or expensive computation
- Suggestion to use `configure-limits` if needed
- Backtrace showing where code was interrupted (when available)

## Rationale

Without timeout protection:
- `(loop)` would hang the server permanently
- Expensive computations could make server unresponsive
- No way to recover without killing server process
- Session state would be lost

Timeout protection ensures server remains usable even when user code has bugs.

This property supports INV-002 (Server Stability) by preventing infinite loops from crashing or hanging the server.

## Counterexample Shape

If this property is violated, you might see:
- Server hangs indefinitely on `(loop)`
- No response to timeout (neither success nor error)
- Server crashes when timeout occurs
- Timeout doesn't produce proper error response
- Subsequent requests fail after timeout
- Output produced before timeout is lost
- Timeout value is ignored

## Verification Approach

**Property Test - Timeout Triggers**:

```lisp
(defun test-timeout-triggers ()
  (let* ((start (get-internal-real-time))
         (result (evaluate "(sleep 5)" :timeout 1))
         (duration (- (get-internal-real-time) start)))
    ;; Should timeout in ~1 second, not wait for sleep to finish
    (assert (< duration (* 2 internal-time-units-per-second)))
    (assert (not (result-success-p result)))
    (assert (search "TIMEOUT" (result-error result)))))
```

**Property Test - Server Survives Timeout**:

```lisp
(defun test-server-survives-timeout ()
  ;; Trigger timeout
  (let ((r1 (evaluate "(loop)" :timeout 1)))
    (assert (not (result-success-p r1))))

  ;; Server should still work
  (let ((r2 (evaluate "(+ 1 2)")))
    (assert (result-success-p r2))
    (assert (equal "3" (result-value r2)))))
```

**Property Test - Partial Output Capture**:

```lisp
(defun test-partial-output-before-timeout ()
  (let ((result (evaluate "(princ \"hello\") (loop)" :timeout 1)))
    (assert (not (result-success-p result)))
    (assert (equal "hello" (result-stdout result)))))
```

**Property Test - Configurable Timeout**:

```lisp
(defun test-configurable-timeout ()
  ;; Set low timeout
  (configure-limits :timeout 1)
  (let ((r1 (evaluate "(sleep 2)")))
    (assert (not (result-success-p r1))))

  ;; Increase timeout
  (configure-limits :timeout 5)
  (let ((r2 (evaluate "(sleep 2)")))
    (assert (result-success-p r2))))
```

**Property Test - Disabled Timeout**:

```lisp
(defun test-disabled-timeout ()
  ;; WARNING: This would hang if there's a bug
  (configure-limits :timeout 0)
  (let ((result (evaluate "(sleep 0.1)")))
    (assert (result-success-p result)))
  ;; Re-enable for safety
  (configure-limits :timeout 30))
```

**Property Test - Error Message Quality**:

```lisp
(defun test-timeout-error-message ()
  (let ((result (evaluate "(loop)" :timeout 1)))
    (let ((msg (result-error result)))
      (assert (search "TIMEOUT" msg))
      (assert (search "configure-limits" msg))
      (assert (search "second" msg)))))
```

**Generator**: Generate code with varying execution times:
- Fast code (< 0.1s)
- Medium code (0.5s - 2s)
- Slow code (> timeout)
- Infinite loops

**Assertion**:
- Fast code completes successfully
- Slow code triggers timeout error
- Server remains operational after timeout
- Timeout precision within acceptable range (T to T+1 seconds)

**Shrinking**: Find minimal code that violates timeout behavior

**Edge Cases**:
- Timeout = 0 (disabled) → no timeout
- Timeout = 1 (minimum) → 1 second limit
- Code that completes exactly at timeout boundary
- Nested timeouts (code that sets timeout then evaluates)

## Related

- [configure-limits-tool](../contracts/configure-limits-tool.md) - Configure timeout
- [timeout-handling scenario](../scenarios/timeout-handling.md) - Test cases
