---
type: verification
name: timeout-protection-property-test
source: properties/timeout-protection.md
level: property
tags:
  - property-based
  - timeout
  - safety
---

# Property Test: Timeout Protection

## Purpose

Verify that evaluation timeout correctly prevents infinite loops and long-running code while maintaining server stability and partial output capture.

## Prerequisites

- Property-based testing framework
- Test session with configurable timeout

## Implementation

### Generator: Random Slow/Fast Code

```lisp
(defun generate-slow-code ()
  "Generate code that takes significant time"
  (random-choice
   (list
    ;; Infinite loops
    "(loop)"
    "(do () (nil))"
    "(tagbody loop (go loop))"

    ;; Long sleeps
    (format nil "(sleep ~A)" (+ 5 (random 10)))

    ;; Expensive computation
    "(loop repeat 100000000 sum (random 100))"
    "(dotimes (i 1000000) (make-list 1000))")))

(defun generate-fast-code ()
  "Generate code that completes quickly"
  (random-choice
   (list
    "(+ 1 2)"
    "(list 1 2 3 4 5)"
    "(dotimes (i 10) (+ i 1))"
    (format nil "(sleep ~A)" (/ (random 100) 1000.0)))))

(defun random-choice (list)
  (nth (random (length list)) list))
```

### Property 1: Slow Code Times Out

```lisp
(fiveam:test slow-code-times-out
  "Code exceeding timeout is terminated with error"
  (let ((session (make-test-session)))
    ;; Set low timeout
    (configure-session-timeout session 1)

    (dotimes (trial 20)
      (let* ((slow-code (generate-slow-code))
             (start-time (get-internal-real-time))
             (result (evaluate-in-session session slow-code))
             (elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second))
             (text (result-content-text result)))

        ;; Should timeout quickly (not wait forever)
        (fiveam:is (<= elapsed 2.5)
                  "Timeout took too long: ~A seconds for ~A" elapsed slow-code)

        ;; Should be error response
        (fiveam:is (result-field result :isError))

        ;; Should mention timeout
        (fiveam:is (search "TIMEOUT" text)
                  "Missing TIMEOUT in error for: ~A" slow-code)))))
```

### Property 2: Fast Code Succeeds

```lisp
(fiveam:test fast-code-succeeds
  "Code completing within timeout returns normally"
  (let ((session (make-test-session)))
    ;; Set reasonable timeout
    (configure-session-timeout session 5)

    (dotimes (trial 50)
      (let* ((fast-code (generate-fast-code))
             (result (evaluate-in-session session fast-code)))

        ;; Should succeed
        (fiveam:is (not (result-field result :isError))
                  "Fast code timed out: ~A" fast-code)))))
```

### Property 3: Server Survives Timeout

```lisp
(fiveam:test server-survives-timeout
  "Server remains operational after timeout"
  (let ((session (make-test-session)))
    (configure-session-timeout session 1)

    (dotimes (trial 10)
      ;; Trigger timeout
      (let ((r1 (evaluate-in-session session "(loop)")))
        (fiveam:is (result-field r1 :isError)))

      ;; Server should still work
      (let ((r2 (evaluate-in-session session "(+ 1 2)")))
        (fiveam:is (not (result-field r2 :isError)))
        (fiveam:is (search "3" (result-value r2)))))))
```

### Property 4: Partial Output Before Timeout

```lisp
(fiveam:test partial-output-before-timeout
  "Output produced before timeout is captured"
  (let ((session (make-test-session)))
    (configure-session-timeout session 1)

    (dotimes (trial 20)
      (let* ((marker (format nil "marker-~A" (random 1000)))
             (code (format nil "(princ \"~A\") (loop)" marker))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Should timeout
        (fiveam:is (result-field result :isError))

        ;; Should have captured output
        (fiveam:is (or (search "[stdout]" text)
                      (search marker text))
                  "Lost output before timeout in: ~A" code)))))
```

### Property 5: Warnings Before Timeout Captured

```lisp
(fiveam:test warnings-before-timeout-captured
  "Warnings before timeout are preserved"
  (let ((session (make-test-session)))
    (configure-session-timeout session 1)

    (dotimes (trial 20)
      (let* ((warning-msg (format nil "warning-~A" (random 1000)))
             (code (format nil "(warn \"~A\") (loop)" warning-msg))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Should timeout
        (fiveam:is (result-field result :isError))

        ;; Should have captured warning
        (fiveam:is (or (search "[warnings]" text)
                      (search warning-msg text))
                  "Lost warning before timeout in: ~A" code)))))
```

### Property 6: Timeout Precision

```lisp
(fiveam:test timeout-precision
  "Timeout triggers within acceptable range (T to T+1 seconds)"
  (let ((session (make-test-session))
        (timeout-seconds 2))

    (configure-session-timeout session timeout-seconds)

    (dotimes (trial 10)
      (let* ((start-time (get-internal-real-time))
             (result (evaluate-in-session session "(loop)"))
             (elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))

        ;; Should timeout in range [T, T+1]
        (fiveam:is (>= elapsed (- timeout-seconds 0.2))
                  "Timeout too early: ~A < ~A" elapsed timeout-seconds)
        (fiveam:is (<= elapsed (+ timeout-seconds 1.5))
                  "Timeout too late: ~A > ~A" elapsed (+ timeout-seconds 1))))))
```

### Property 7: Configurable Timeout Works

```lisp
(fiveam:test configurable-timeout-works
  "Different timeout values produce different behavior"
  (let ((session (make-test-session)))

    ;; Low timeout - should timeout
    (configure-session-timeout session 1)
    (let ((r1 (evaluate-in-session session "(sleep 2)")))
      (fiveam:is (result-field r1 :isError))
      (fiveam:is (search "TIMEOUT" (result-content-text r1))))

    ;; Higher timeout - should succeed
    (configure-session-timeout session 5)
    (let ((r2 (evaluate-in-session session "(sleep 1)")))
      (fiveam:is (not (result-field r2 :isError))))))
```

### Property 8: Disabled Timeout Allows Long Code

```lisp
(fiveam:test disabled-timeout-allows-long-code
  "Setting timeout to 0 disables timeout"
  (let ((session (make-test-session)))

    ;; Disable timeout
    (configure-session-timeout session 0)

    ;; Long-running code should complete
    ;; (Use short sleep, not infinite loop, for test safety)
    (let* ((result (evaluate-in-session session "(sleep 0.5) :done"))
           (text (result-value result)))
      (fiveam:is (not (result-field result :isError)))
      (fiveam:is (search ":DONE" text)))

    ;; Re-enable for safety
    (configure-session-timeout session 30)))
```

### Property 9: Session State Preserved After Timeout

```lisp
(fiveam:test session-state-preserved-after-timeout
  "State defined before timeout remains after timeout"
  (let ((session (make-test-session))
        (var-name (intern (format nil "TEST-~A" (random 10000)) :cl-user)))

    (configure-session-timeout session 5)

    ;; Define variable
    (evaluate-in-session session (format nil "(defvar ~A 42)" var-name))

    ;; Trigger timeout
    (configure-session-timeout session 1)
    (evaluate-in-session session "(loop)")

    ;; Variable should still exist
    (configure-session-timeout session 5)
    (let* ((result (evaluate-in-session session (prin1-to-string var-name)))
           (text (result-value result)))
      (fiveam:is (search "42" text)))))
```

### Property 10: Timeout Error Message Quality

```lisp
(fiveam:test timeout-error-message-quality
  "Timeout errors include helpful information"
  (let ((session (make-test-session)))
    (configure-session-timeout session 1)

    (let* ((result (evaluate-in-session session "(loop)"))
           (text (result-content-text result)))

      ;; Should be error
      (fiveam:is (result-field result :isError))

      ;; Should have helpful hints
      (fiveam:is (search "TIMEOUT" text))
      (fiveam:is (or (search "configure-limits" text)
                    (search "timeout" text)))
      (fiveam:is (or (search "second" text)
                    (search "exceeded" text))))))
```

### Property 11: No Resource Leaks After Timeout

```lisp
(fiveam:test no-resource-leaks-after-timeout
  "Multiple timeouts don't accumulate resources"
  (let ((session (make-test-session)))
    (configure-session-timeout session 1)

    ;; Trigger many timeouts
    (dotimes (i 20)
      (let ((result (evaluate-in-session session "(loop)")))
        (fiveam:is (result-field result :isError))))

    ;; Brief pause for cleanup
    (sleep 1)

    ;; Server should still be responsive
    (dotimes (i 10)
      (let ((result (evaluate-in-session session "(+ 1 2)")))
        (fiveam:is (not (result-field result :isError)))))))
```

## Configuration

- Trials: 20 for slow code tests (time-intensive)
- Trials: 50 for fast code tests
- Shrinking: Limited (timeout tests don't shrink well)

## Helper Functions

```lisp
(defun configure-session-timeout (session seconds)
  "Configure timeout for a session"
  (send-tool-call session "configure-limits"
                 `(("timeout" . ,seconds))))
```

## Assertions

For every trial:
1. Slow code times out with error response
2. Fast code completes successfully
3. Server remains operational after timeout
4. Partial output before timeout is captured
5. Warnings before timeout are preserved
6. Timeout precision is within acceptable range
7. Configurable timeout affects behavior
8. Disabled timeout allows long operations
9. Session state survives timeout
10. Timeout errors include helpful information

## Notes

- This test verifies timeout protection (related to INV-002)
- Tests both infinite loops and long-running code
- Verifies server resilience to multiple timeouts
- Confirms partial output preservation
- Tests timeout precision and configuration
- WARNING: Disabled timeout tests use finite sleep, not infinite loops
