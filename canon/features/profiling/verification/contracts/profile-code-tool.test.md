---
type: verification
name: profile-code-contract-test
source: contracts/profile-code-tool.md
level: contract
tags:
  - profiling
  - performance
---

# Contract Test: Profile-Code Tool

## Purpose

Verify the `profile-code` tool correctly profiles code execution, identifies hot spots, and reports timing and allocation statistics.

## Prerequisites

- Initialized MCP server with profiling tools
- Code that runs long enough to profile (>0.5s)

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)
```

## Test Cases

### Basic CPU Profiling

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(defun slow-fib (n) (if (<= n 1) n (+ (slow-fib (- n 1)) (slow-fib (- n 2))))) (slow-fib 25)",
    "mode": "cpu"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Profile report present
    (assert (search "Profile" content :test #'char-equal))

    ;; Sample count
    (assert (search "samples" content :test #'char-equal))

    ;; Function names
    (assert (search "SLOW-FIB" content :test #'char-equal))

    ;; Percentages
    (assert (search "%" content))

    ;; Result preserved
    (assert (or (search "Result" content :test #'char-equal)
                (search "result" content)))))
```

### Flat Report Format

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(loop repeat 10000000 sum (random 100))",
    "mode": "cpu",
    "report-type": "flat"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Flat format columns
    (assert (search "Function" content :test #'char-equal))
    (assert (search "Samples" content :test #'char-equal))
    (assert (search "%" content))

    ;; RANDOM should be hot spot
    (assert (search "RANDOM" content :test #'char-equal))))
```

### Graph Report Format

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(defun a () (loop repeat 1000000 sum (b))) (defun b () (random 100)) (a)",
    "mode": "cpu",
    "report-type": "graph"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Graph format
    (assert (or (search "Call Graph" content :test #'char-equal)
                (search "graph" content :test #'char-equal)))

    ;; Call hierarchy
    (assert (search "A" content :test #'char-equal))
    (assert (search "B" content :test #'char-equal))))
```

### Allocation Profiling

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(loop repeat 10000 collect (make-list 100))",
    "mode": "alloc"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Allocation mode
    (assert (search "ALLOC" content :test #'char-equal))

    ;; Bytes or MB mentioned
    (assert (or (search "bytes" content :test #'char-equal)
                (search "MB" content)
                (search "KB" content)))

    ;; MAKE-LIST should be hot spot
    (assert (search "MAKE-LIST" content :test #'char-equal))))
```

### Time (Wall-Clock) Profiling

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(dotimes (i 5) (sleep 0.1) (loop repeat 100000 sum i))",
    "mode": "time"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Time mode
    (assert (or (search "time" content :test #'char-equal)
                (search "TIME" content)))

    ;; Profile data present
    (assert (search "samples" content :test #'char-equal))

    ;; Should show sleep or loop
    (assert (or (search "SLEEP" content :test #'char-equal)
                (search "DOTIMES" content :test #'char-equal)))))
```

### Fast Code Warning

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(+ 1 2 3)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Should warn about insufficient samples or fast execution
    (assert (or (search "Warning" content :test #'char-equal)
                (search "too quickly" content :test #'char-equal)
                (search "samples: 0" content :test #'char-equal)
                (search "fast" content :test #'char-equal)))

    ;; Result still present
    (assert (or (search "Result" content :test #'char-equal)
                (search "6" content)))))
```

### Custom Sample Parameters

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(loop repeat 5000000 sum (random 100))",
    "max-samples": 500,
    "sample-interval": 0.02
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Profile completed
    (assert (search "Profile" content :test #'char-equal))

    ;; Should have samples (up to 500)
    (assert (search "samples" content :test #'char-equal))))
```

### Error During Profiling

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(loop repeat 1000000 do (if (zerop (mod i 500000)) (/ 1 0)))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  ;; Should capture error
  (assert (or (search "error" (result-content response) :test #'char-equal)
              (search "DIVISION-BY-ZERO" (result-content response) :test #'char-equal)
              (error-response-p response))))
```

### Package Context

**Input**:
```json
{
  "name": "profile-code",
  "arguments": {
    "code": "(loop repeat 1000000 sum 1)",
    "package": "CL-USER"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "profile-code" input)))
  (assert (result-response-p response))
  (assert (search "Profile" (result-content response) :test #'char-equal)))
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Notes

- Profiling requires code to run at least ~0.5 seconds for meaningful results
- Sample counts vary based on execution time and sample interval
- Hot spots appear as functions with high sample counts
- Different modes (cpu, time, alloc) show different perspectives
- Very fast code may produce insufficient samples
- Report formats (flat, graph) organize data differently
