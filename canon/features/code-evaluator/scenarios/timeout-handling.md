---
type: scenario
name: timeout-handling
version: 0.1.0
---

# Timeout Handling Scenarios

Scenarios demonstrating evaluation timeout behavior.

## Scenario 1: Fast Code Succeeds

Code that completes within timeout returns normally.

**Given**: Default timeout of 30 seconds
**When**: Evaluating `(+ 1 2)`
**Then**: Returns success with value `3`

```lisp
(let ((result (evaluate-code "(+ 1 2)")))
  (assert (result-success-p result))
  (assert (equal '("3") (result-values result))))
```

## Scenario 2: Slow Code Times Out

Code exceeding timeout produces structured error.

**Given**: Timeout of 1 second
**When**: Evaluating `(sleep 3)`
**Then**: Returns error with timeout information

```lisp
(let ((result (evaluate-code "(sleep 3)" :timeout 1)))
  (assert (not (result-success-p result)))
  (assert (search "TIMEOUT" (result-error result))))
```

## Scenario 3: Timeout Error Contains Helpful Hints

The error message guides users toward resolution.

**Given**: Any timeout
**Then**: Error includes:
  - The timeout duration exceeded
  - Hint about infinite loops or expensive computation
  - Suggestion to use `configure-limits`

```lisp
(let ((result (evaluate-code "(loop)" :timeout 1)))
  (assert (search "configure-limits" (result-error result))))
```

## Scenario 4: Output Captured Before Timeout

Partial output is preserved even when timeout occurs.

**Given**: Code that prints before entering infinite loop
**When**: Timeout triggers during loop
**Then**: Printed output is captured

```lisp
(let ((result (evaluate-code "(princ \"hello\") (loop)" :timeout 1)))
  (assert (equal "hello" (result-stdout result)))
  (assert (not (result-success-p result))))
```

## Scenario 5: Warnings Captured Before Timeout

Warnings signaled before timeout are preserved.

**Given**: Code that warns before entering infinite loop
**When**: Timeout triggers during loop
**Then**: Warning is captured

```lisp
(let ((result (evaluate-code "(warn \"caution\") (loop)" :timeout 1)))
  (assert (= 1 (length (result-warnings result))))
  (assert (not (result-success-p result))))
```

## Scenario 6: Disabled Timeout

Setting timeout to NIL allows unlimited execution.

**Given**: Timeout disabled
**When**: Running any code
**Then**: No timeout occurs (code runs to completion or forever)

```lisp
(let ((result (evaluate-code "(sleep 0.1)" :timeout nil)))
  (assert (result-success-p result)))
```

**Warning**: Only use with trusted code. Infinite loops will hang the server.

## Scenario 7: Configurable Timeout

Timeout can be adjusted via `configure-limits` tool.

**Given**: Default timeout of 30 seconds
**When**: Calling `configure-limits` with `{"timeout": 60}`
**Then**: `*evaluation-timeout*` becomes 60

```lisp
(call-tool "configure-limits" '(("timeout" . 60)) session)
(assert (= 60 *evaluation-timeout*))
```

## Non-Functional Requirements

### NFR-1: Timeout Precision

Timeout should trigger within reasonable precision:
- For timeout T seconds, actual termination should be within T to T+1 seconds
- Threading and condition system overhead is acceptable

### NFR-2: No Resource Leaks

After timeout:
- Evaluation thread should be cleanly terminated
- No orphaned threads or open resources
- Session state should remain consistent

### NFR-3: Backtrace Quality

When available, backtrace should show:
- Function being executed when interrupted
- Call stack context
- Sufficient depth to identify the problem
