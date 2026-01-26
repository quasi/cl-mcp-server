---
type: verification
name: time-execution-tool-contract-test
source: contracts/time-execution-tool.md
level: contract
tags:
  - profiling
  - phase-b
---

# Contract Test: Time-Execution Tool

## Purpose

Verify the time-execution tool correctly measures execution time and memory allocation with detailed profiling output.

## Prerequisites

- Initialized server with MCP session
- Phase B features enabled

## Setup

```lisp
(defvar *test-session* (cl-mcp-server-tests:make-test-session))
```

## Test Cases

### Basic Timing

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(+ 1 2)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(+ 1 2)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))

  ;; Must have timing section
  (assert (search "Timing:" text))
  (assert (search "Real time:" text))
  (assert (search "Run time:" text))
  (assert (search "GC time:" text))
  (assert (search "Bytes consed:" text))
  (assert (search "ms" text))

  ;; Must have result section
  (assert (search "Result:" text))
  (assert (search "3" text)))
```

### Timing with Output

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(format t \"Computing...~%\") (+ 1 2)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(format t \"Computing...~%\") (+ 1 2)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Timing:" text))
  (assert (search "Result:" text))
  (assert (search "Output:" text))
  (assert (search "Computing..." text)))
```

### Timing Memory Allocation

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(make-list 1000)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(make-list 1000)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Bytes consed:" text))
  ;; Should show non-zero allocation
  (let ((bytes-line (find-line-containing text "Bytes consed:")))
    (assert (not (search "0" bytes-line)))))
```

### Timing with Sleep

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(sleep 0.1) :done"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(sleep 0.1) :done"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Real time:" text))
  ;; Real time should be ~100ms or more
  (let ((real-time-line (find-line-containing text "Real time:")))
    ;; Extract number from "Real time: 102 ms"
    (let ((ms (parse-ms-from-line real-time-line)))
      (assert (>= ms 80)))))  ; Allow some variance
```

### Package Context

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(length '(a b c))",
      "package": "CL-USER"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(length '(a b c))")
                                   ("package" . "CL-USER"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Timing:" text))
  (assert (search "Result:" text))
  (assert (search "3" text)))
```

### Error During Execution

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(/ 1 0)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(/ 1 0)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (search "Error during execution" text))
  (assert (search "DIVISION-BY-ZERO" text)))
```

### Timing Expensive Computation

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {
      "code": "(dotimes (i 10000) (+ i 1))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(dotimes (i 10000) (+ i 1))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Timing:" text))
  ;; Run time should be measurable
  (let ((run-time-line (find-line-containing text "Run time:")))
    (assert run-time-line)))
```

### Missing Required Parameter

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "time-execution",
    "arguments": {}
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "time-execution" '())))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602)))  ; Invalid params
```

### Timing Format Consistency

**Verify all timing metrics are present and formatted correctly**:

```lisp
(let* ((response (send-tool-call *test-session* "time-execution"
                                 '(("code" . "(loop repeat 100 collect (cons 1 2))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))

  ;; Check all metrics present
  (assert (find-line-containing text "Real time:"))
  (assert (find-line-containing text "Run time:"))
  (assert (find-line-containing text "GC time:"))
  (assert (find-line-containing text "Bytes consed:"))

  ;; Check formatting
  (let ((real (find-line-containing text "Real time:"))
        (run (find-line-containing text "Run time:"))
        (gc (find-line-containing text "GC time:")))
    ;; All should have "ms" unit
    (assert (search "ms" real))
    (assert (search "ms" run))
    (assert (search "ms" gc))))
```

### Comparison: time-execution vs evaluate-lisp

**Verify time-execution focuses on profiling**:

```lisp
;; Regular evaluation
(let* ((eval-response (send-tool-call *test-session* "evaluate-lisp"
                                     '(("code" . "(+ 1 2)"))))
       (eval-text (result-content-text eval-response)))
  ;; Should just have result
  (assert (search "=> 3" eval-text))
  (assert (not (search "Timing:" eval-text))))

;; Timed execution
(let* ((time-response (send-tool-call *test-session* "time-execution"
                                     '(("code" . "(+ 1 2)"))))
       (time-text (result-content-text time-response)))
  ;; Should have timing first, then result
  (assert (search "Timing:" time-text))
  (assert (search "Result:" time-text))
  (assert (< (search "Timing:" time-text)
             (search "Result:" time-text))))
```

## Teardown

```lisp
(cl-mcp-server-tests:cleanup-test-session *test-session*)
```

## Notes

- All timing values are in milliseconds
- Bytes consed includes all allocations, including short-lived objects
- GC time is cumulative across all GC runs during execution
- Real time includes I/O and scheduling overhead
- Run time is actual CPU time used by the process
- Output format is structured for profiling analysis
