---
type: verification
name: timeout-handling-scenario-test
source: scenarios/timeout-handling.md
level: scenario
tags:
  - integration
  - timeout
  - safety
---

# Scenario Test: Timeout Handling

## Purpose

Verify that evaluation timeout correctly prevents infinite loops and long-running code while preserving partial output and server stability.

## Prerequisites

- Initialized server with MCP session
- Default timeout configuration (30 seconds)

## Setup

```lisp
(defvar *test-server* (cl-mcp-server:start-test-server))

(defun eval-code (id code)
  (send-request *test-server*
               `((:jsonrpc . "2.0")
                 (:id . ,id)
                 (:method . "tools/call")
                 (:params . ((:name . "evaluate-lisp")
                            (:arguments . ((:code . ,code))))))))

(defun configure-timeout (seconds)
  (send-request *test-server*
               `((:jsonrpc . "2.0")
                 (:id . 0)
                 (:method . "tools/call")
                 (:params . ((:name . "configure-limits")
                            (:arguments . ((:timeout . ,seconds))))))))
```

## Execution

### Scenario 1: Fast Code Succeeds Within Timeout

```lisp
;; Set reasonable timeout
(configure-timeout 5)

(let* ((response (eval-code 1 "(+ 1 2)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "=> 3" text)))
```

### Scenario 2: Slow Code Times Out

```lisp
;; Set low timeout
(configure-timeout 1)

(let* ((start-time (get-internal-real-time))
       (response (eval-code 2 "(sleep 3)"))
       (elapsed (/ (- (get-internal-real-time) start-time)
                   internal-time-units-per-second))
       (text (result-content-text response)))

  ;; Should timeout, not wait for completion
  (assert (<= elapsed 2.0))  ; Should timeout around 1 second, not 3

  ;; Should be error response
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (search "TIMEOUT" text)))
```

### Scenario 3: Infinite Loop Times Out

```lisp
;; Set timeout
(configure-timeout 1)

(let* ((start-time (get-internal-real-time))
       (response (eval-code 3 "(loop)"))
       (elapsed (/ (- (get-internal-real-time) start-time)
                   internal-time-units-per-second))
       (text (result-content-text response)))

  ;; Should timeout quickly
  (assert (<= elapsed 2.0))

  ;; Should be error response
  (assert (result-field response :isError))
  (assert (search "TIMEOUT" text)))
```

### Scenario 4: Timeout Error Message Contains Hints

```lisp
(configure-timeout 1)

(let* ((response (eval-code 4 "(loop)"))
       (text (result-content-text response)))
  (assert (result-field response :isError))

  ;; Should mention configure-limits
  (assert (search "configure-limits" text))

  ;; Should mention timeout/seconds
  (assert (or (search "second" text) (search "timeout" text))))
```

### Scenario 5: Output Captured Before Timeout

```lisp
(configure-timeout 1)

(let* ((response (eval-code 5 "(princ \"hello\") (princ \"world\") (loop)"))
       (text (result-content-text response)))
  (assert (result-field response :isError))

  ;; Partial output should be captured
  (assert (search "[stdout]" text))
  (assert (search "hello" text))
  (assert (search "world" text))

  ;; Still a timeout error
  (assert (search "TIMEOUT" text)))
```

### Scenario 6: Warnings Captured Before Timeout

```lisp
(configure-timeout 1)

(let* ((response (eval-code 6 "(warn \"caution\") (loop)"))
       (text (result-content-text response)))
  (assert (result-field response :isError))

  ;; Warning should be captured
  (assert (search "[warnings]" text))
  (assert (search "caution" text))

  ;; Still a timeout error
  (assert (search "TIMEOUT" text)))
```

### Scenario 7: Configurable Timeout

```lisp
;; Set low timeout - should timeout
(configure-timeout 1)
(let ((response (eval-code 7 "(sleep 2)")))
  (assert (result-field response :isError))
  (assert (search "TIMEOUT" (result-content-text response))))

;; Increase timeout - should succeed
(configure-timeout 5)
(let ((response (eval-code 8 "(sleep 1)")))
  (assert (not (result-field response :isError))))
```

### Scenario 8: Server Survives Timeout

```lisp
;; Trigger timeout
(configure-timeout 1)
(let ((response (eval-code 9 "(loop)")))
  (assert (result-field response :isError)))

;; Server should still be operational
(let* ((response (eval-code 10 "(+ 1 2)"))
       (text (result-content-text response)))
  (assert (not (result-field response :isError)))
  (assert (search "=> 3" text)))
```

### Scenario 9: Disabled Timeout Allows Long Operations

```lisp
;; Disable timeout (WARNING: Use with trusted code only)
(configure-timeout 0)

(let* ((response (eval-code 11 "(sleep 0.5) :done"))
       (text (result-content-text response)))
  (assert (not (result-field response :isError)))
  (assert (search "=> :DONE" text)))

;; Re-enable for safety
(configure-timeout 30)
```

### Scenario 10: Timeout Precision

```lisp
;; Measure timeout precision
(configure-timeout 2)

(let* ((start-time (get-internal-real-time))
       (response (eval-code 12 "(sleep 10)"))
       (elapsed (/ (- (get-internal-real-time) start-time)
                   internal-time-units-per-second)))

  ;; Should timeout within acceptable range: T to T+1 seconds
  (assert (<= 2.0 elapsed))
  (assert (<= elapsed 3.5))

  (assert (result-field response :isError)))
```

## Verification

### Server Stability After Multiple Timeouts

```lisp
(configure-timeout 1)

;; Trigger multiple timeouts
(dotimes (i 5)
  (let ((response (eval-code (+ 100 i) "(loop)")))
    (assert (result-field response :isError))))

;; Server should still work
(let* ((response (eval-code 200 "(list 1 2 3)"))
       (text (result-content-text response)))
  (assert (not (result-field response :isError)))
  (assert (search "(1 2 3)" text)))
```

### Session State Preserved After Timeout

```lisp
(configure-timeout 5)

;; Set variable
(eval-code 201 "(defvar *test-var* 42)")

;; Trigger timeout
(configure-timeout 1)
(eval-code 202 "(loop)")

;; Variable should still exist
(configure-timeout 5)
(let* ((response (eval-code 203 "*test-var*"))
       (text (result-content-text response)))
  (assert (search "=> 42" text)))
```

### Timeout Configuration Persists

```lisp
;; Set timeout
(configure-timeout 3)

;; Do some work
(eval-code 300 "(+ 1 2)")
(eval-code 301 "(* 3 4)")

;; Query configuration
(let* ((response (send-request *test-server*
                              '((:jsonrpc . "2.0")
                                (:id . 302)
                                (:method . "tools/call")
                                (:params . ((:name . "configure-limits")
                                           (:arguments . ()))))))
       (text (result-content-text response)))
  ;; Should still be 3 seconds
  (assert (search "timeout: 3 seconds" text)))
```

### No Resource Leaks After Timeout

```lisp
(configure-timeout 1)

;; Trigger multiple timeouts
(dotimes (i 10)
  (eval-code (+ 400 i) "(loop)"))

;; All should have timed out, none should hang
;; Memory and threads should be cleaned up
;; (This is more of a manual inspection test)
(sleep 2)  ; Give time for cleanup

;; Server should be responsive
(let ((response (eval-code 500 ":ok")))
  (assert (not (result-field response :isError))))
```

## Teardown

```lisp
;; Restore default timeout
(configure-timeout 30)

(cl-mcp-server:stop-test-server *test-server*)
```

## Notes

- This scenario verifies timeout protection (related to INV-002)
- Timeouts prevent infinite loops from hanging the server
- Partial output before timeout is preserved
- Server remains stable after timeouts
- Timeout precision is within acceptable range (T to T+1 seconds)
- Session state is preserved after timeout
- Configure-limits allows adjusting timeout for long operations
- Disabling timeout (0) allows unlimited execution (use with caution)
