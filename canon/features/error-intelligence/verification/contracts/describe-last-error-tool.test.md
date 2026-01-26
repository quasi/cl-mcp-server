---
type: verification
name: describe-last-error-contract-test
source: contracts/describe-last-error-tool.md
level: contract
tags:
  - error-intelligence
  - error-handling
---

# Contract Test: Describe-Last-Error Tool

## Purpose

Verify the `describe-last-error` tool correctly captures and reports error information including type, message, restarts, and backtrace preview.

## Prerequisites

- Initialized MCP server with error-intelligence tools
- Ability to trigger various error types

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)
```

## Test Cases

### Error Information After Failure

**Setup**: Trigger an error
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(/ 1 0)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Error type present
    (assert (search "DIVISION-BY-ZERO" content :test #'char-equal))

    ;; Error message present
    (assert (search "arithmetic error" content :test #'char-equal))

    ;; Restarts section present
    (assert (search "Available Restarts" content :test #'char-equal))
    (assert (search "ABORT" content :test #'char-equal))

    ;; Backtrace preview present
    (assert (search "Backtrace" content :test #'char-equal))
    (assert (search "frames" content :test #'char-equal))))
```

### No Error Available

**Input** (without prior error):
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Appropriate message
    (assert (or (search "No error" content :test #'char-equal)
                (search "not available" content :test #'char-equal)))))
```

### Type Error Information

**Setup**: Trigger type error
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(car 'not-a-list)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Type error detected
    (assert (search "TYPE-ERROR" content :test #'char-equal))

    ;; Mentions the problematic value
    (assert (or (search "NOT-A-LIST" content :test #'char-equal)
                (search "SYMBOL" content :test #'char-equal)))))
```

### Undefined Function Error

**Setup**: Call undefined function
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(nonexistent-function 42)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Undefined function error
    (assert (search "UNDEFINED-FUNCTION" content :test #'char-equal))

    ;; Function name mentioned
    (assert (search "NONEXISTENT-FUNCTION" content :test #'char-equal))

    ;; May have restart options
    (when (search "Restarts" content :test #'char-equal)
      (assert (or (search "USE-VALUE" content :test #'char-equal)
                  (search "RETRY" content :test #'char-equal))))))
```

### User-Signaled Error

**Setup**: Signal custom error
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(error \"Custom error message\")")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Simple error type
    (assert (search "SIMPLE-ERROR" content :test #'char-equal))

    ;; Custom message present
    (assert (search "Custom error message" content :test #'char-equal))))
```

### Backtrace Preview Limit

**Setup**: Error in nested calls
```lisp
(call-tool *server* "evaluate-lisp"
          '(("code" . "(defun a () (/ 1 0))
                       (defun b () (a))
                       (defun c () (b))
                       (c)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Backtrace section present
    (assert (search "Backtrace" content :test #'char-equal))

    ;; Top frames shown (typically 5)
    (assert (search "top" content :test #'char-equal))

    ;; Mention of full backtrace tool
    (assert (or (search "get-backtrace" content :test #'char-equal)
                (search "full backtrace" content :test #'char-equal)))))
```

### Error Persistence Across Calls

**Setup**: Trigger error
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(/ 1 0)")))
```

**Test**: Call describe-last-error multiple times
```lisp
(let ((response1 (call-tool *server* "describe-last-error" '()))
      (response2 (call-tool *server* "describe-last-error" '())))

  ;; Both should report same error
  (assert (search "DIVISION-BY-ZERO" (result-content response1) :test #'char-equal))
  (assert (search "DIVISION-BY-ZERO" (result-content response2) :test #'char-equal)))
```

### Error Cleared by Success

**Setup**: Error then successful eval
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(/ 1 0)")))
(call-tool *server* "evaluate-lisp" '(("code" . "(+ 1 2 3)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  ;; May still show last error OR show "no error"
  ;; Implementation choice - both acceptable
  (assert t))
```

### Restart Information Present

**Setup**: Trigger error that has restarts
```lisp
(call-tool *server* "evaluate-lisp" '(("code" . "(error 'simple-error)")))
```

**Input**:
```json
{
  "name": "describe-last-error",
  "arguments": {}
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "describe-last-error" '())))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Restarts section
    (assert (search "Restart" content :test #'char-equal))

    ;; At least ABORT restart
    (assert (search "ABORT" content :test #'char-equal))))
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Notes

- Error information must persist across multiple tool calls
- All error components (type, message, restarts, backtrace) required
- Backtrace preview typically shows top 5 frames
- No error case must be handled gracefully
- Error cleared by successful evaluation (implementation choice)
