---
type: verification
name: configure-limits-tool-contract-test
source: contracts/configure-limits-tool.md
level: contract
tags:
  - configuration
  - limits
---

# Contract Test: Configure-Limits Tool

## Purpose

Verify the configure-limits tool correctly adjusts evaluation safety parameters and reports current configuration.

## Prerequisites

- Initialized server with MCP session
- Default limits in place

## Setup

```lisp
(defvar *test-session* (cl-mcp-server-tests:make-test-session))
;; Save initial configuration to restore later
(defvar *initial-config* (call-tool "configure-limits" '() *test-session*))
```

## Test Cases

### Query Current Configuration

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {}
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "configure-limits" '()))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "Current limits:" text))
  (assert (search "timeout:" text))
  (assert (search "max-output:" text))
  (assert (search "seconds" text))
  (assert (search "characters" text)))
```

### Set Timeout

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "timeout": 60
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "configure-limits"
                                 '(("timeout" . 60))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "timeout: 60 seconds" text)))
```

### Set Max Output

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "max-output": 50000
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "configure-limits"
                                 '(("max-output" . 50000))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "max-output: 50000 characters" text)))
```

### Set Both Parameters

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "timeout": 120,
      "max-output": 200000
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "configure-limits"
                                 '(("timeout" . 120)
                                   ("max-output" . 200000))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "timeout: 120 seconds" text))
  (assert (search "max-output: 200000 characters" text)))
```

### Disable Timeout (Warning Case)

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "timeout": 0
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "configure-limits"
                                 '(("timeout" . 0))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (or (search "timeout: disabled" text)
              (search "timeout: 0 seconds" text)))
  (assert (search "WARNING" text)))
```

### Verify Timeout Actually Applied

**Test that timeout setting affects evaluation**:

```lisp
;; Set very low timeout
(send-tool-call *test-session* "configure-limits" '(("timeout" . 1)))

;; Try code that takes longer
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(sleep 3)"))))
       (text (result-content-text response)))
  ;; Should timeout
  (assert (result-field response :isError))
  (assert (search "TIMEOUT" text)))

;; Increase timeout
(send-tool-call *test-session* "configure-limits" '(("timeout" . 5)))

;; Same code should now succeed
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(sleep 1)"))))
       (text (result-content-text response)))
  (assert (not (result-field response :isError))))
```

### Verify Max Output Actually Applied

**Test that max-output setting affects evaluation**:

```lisp
;; Set low output limit
(send-tool-call *test-session* "configure-limits" '(("max-output" . 100)))

;; Generate more output than limit
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(dotimes (i 100) (princ \"abcdefghij\"))"))))
       (text (result-content-text response)))
  ;; Output should be truncated
  (assert (< (length text) 200))  ; Much less than 1000 chars we tried to print
  (assert (or (search "truncated" text)
              (search "exceeded" text))))
```

### Invalid Timeout (Negative)

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "timeout": -5
    }
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "configure-limits"
                                '(("timeout" . -5)))))
  (assert (result-response-p response))
  (assert (result-field response :isError)))
```

### Invalid Max Output (Negative)

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "configure-limits",
    "arguments": {
      "max-output": -1000
    }
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "configure-limits"
                                '(("max-output" . -1000)))))
  (assert (result-response-p response))
  (assert (result-field response :isError)))
```

### Settings Persist Across Evaluations

**Test that configuration changes persist**:

```lisp
;; Set custom limits
(send-tool-call *test-session* "configure-limits"
               '(("timeout" . 45) ("max-output" . 75000)))

;; Do some evaluations
(send-tool-call *test-session* "evaluate-lisp" '(("code" . "(+ 1 2)")))
(send-tool-call *test-session* "evaluate-lisp" '(("code" . "(* 3 4)")))

;; Query config again - should still have custom values
(let* ((response (send-tool-call *test-session* "configure-limits" '()))
       (text (result-content-text response)))
  (assert (search "timeout: 45 seconds" text))
  (assert (search "max-output: 75000 characters" text)))
```

## Teardown

```lisp
;; Restore initial configuration
(when *initial-config*
  (send-tool-call *test-session* "configure-limits"
                 (extract-config-params *initial-config*)))

(cl-mcp-server-tests:cleanup-test-session *test-session*)
```

## Notes

- Changes are global to the session
- Settings persist until changed or session ends
- Default timeout is 30 seconds
- Default max-output is 100000 characters
- Setting timeout to 0 disables it (not recommended)
- Tool always returns current configuration after changes
