---
type: verification
name: time-execution-contract-test
source: contracts/time-execution-tool.md
level: contract
tags:
  - enhanced-evaluation
  - profiling
  - timing
---

# Contract Test: Time-Execution Tool

## Purpose

Verify the `time-execution` tool accurately measures execution time, memory allocation, and GC statistics while executing code.

## Prerequisites

- Initialized MCP server with enhanced-evaluation tools
- Ability to capture timing measurements

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)
```

## Test Cases

### Simple Computation

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(+ 1 2 3)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Result value present
  (assert (search "\"result\": \"6\"" (result-content response)))

  ;; Timing fields present
  (let ((content (result-content response)))
    (assert (search "real-time-ms" content))
    (assert (search "run-time-ms" content))
    (assert (search "gc-time-ms" content))
    (assert (search "bytes-allocated" content))
    (assert (search "allocation-rate" content))

    ;; Times are positive (even if small)
    (assert (search-for-positive-number content "real-time-ms"))

    ;; Simple arithmetic should allocate nothing
    (assert (or (search "\"bytes-allocated\": 0" content)
                (search "\"bytes-allocated\": 16" content))))) ; Some overhead ok
```

### Sleep Timing Accuracy

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(sleep 0.1)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Extract real-time
  (let ((real-time (extract-number (result-content response) "real-time-ms")))
    ;; Should be approximately 100ms (±10ms tolerance)
    (assert (>= real-time 95.0))
    (assert (<= real-time 115.0))))
```

### Allocation Tracking

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(make-list 10000)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Should allocate significant memory (cons cells)
  (let ((bytes-allocated (extract-number (result-content response) "bytes-allocated")))
    ;; 10000 cons cells = ~160KB (16 bytes per cons on 64-bit)
    (assert (> bytes-allocated 100000))))
```

### CPU-Bound Work

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(loop repeat 1000000 sum 1)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Should have non-trivial run-time
  (let ((run-time (extract-number (result-content response) "run-time-ms")))
    (assert (> run-time 0.5))))
```

### Output Capture

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(dotimes (i 3) (print i))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Output captured
  (assert (search "\"output\":" (result-content response)))
  (assert (search "0" (result-content response)))
  (assert (search "1" (result-content response)))
  (assert (search "2" (result-content response))))
```

### Error During Execution

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(/ 1 0)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  ;; Error reported
  (assert (result-response-p response))
  (assert (search "DIVISION-BY-ZERO" (result-content response) :test #'char-equal))

  ;; Timing still captured
  (assert (search "timing" (result-content response)))
  (assert (search "real-time-ms" (result-content response))))
```

### GC Time Tracking

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(progn (gc :full t) (loop repeat 100000 collect (cons 1 2)))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; GC time should be measurable (may be zero if GC fast)
  (assert (search "gc-time-ms" (result-content response)))

  ;; Significant allocation
  (let ((bytes (extract-number (result-content response) "bytes-allocated")))
    (assert (> bytes 1000000)))) ; 100k cons = ~1.6MB
```

### Package Context

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(package-name *package*)",
    "package": "CL-USER"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))
  (assert (search "CL-USER" (result-content response))))
```

### Invalid Package

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(+ 1 2)",
    "package": "NONEXISTENT-PACKAGE"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (error-response-p response))
  (assert (search "package" (error-message response) :test #'char-equal)))
```

### Multiple Forms

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(setf x 10) (setf y 20) (+ x y)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Returns last value
  (assert (search "\"result\": \"30\"" (result-content response))))
```

### Large Result Truncation

**Input**:
```json
{
  "name": "time-execution",
  "arguments": {
    "code": "(make-list 100000)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "time-execution" input)))
  (assert (result-response-p response))

  ;; Result should be truncated
  (let ((content (result-content response)))
    (assert (or (search "..." content)
                (< (length content) 20000))))) ; Not megabytes of output
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Helper Functions

```lisp
(defun extract-number (text field-name)
  "Extract numeric value from JSON-like text"
  (let* ((start (search field-name text))
         (colon-pos (position #\: text :start start))
         (comma-pos (or (position #\, text :start colon-pos)
                       (length text))))
    (parse-number (subseq text colon-pos comma-pos))))

(defun search-for-positive-number (text field-name)
  "Check if field has positive number value"
  (let ((value (extract-number text field-name)))
    (> value 0)))
```

## Notes

- Timing accuracy tested with sleep (real-time) and computation (run-time)
- Allocation tracking verified with known allocation patterns
- Error handling preserves timing data
- All timing fields must be present in response
- GC time may be zero for fast GC or no GC cycles
