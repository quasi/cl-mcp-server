---
type: verification
name: evaluate-lisp-tool-contract-test
source: contracts/evaluate-lisp-tool.md
level: contract
tags:
  - smoke
  - evaluation
---

# Contract Test: Evaluate-Lisp Tool

## Purpose

Verify the evaluate-lisp tool correctly implements its contract for code evaluation, output capture, and error handling.

## Prerequisites

- Initialized server with MCP session
- Fresh evaluation session or reset session state

## Setup

```lisp
(defvar *test-session* (cl-mcp-server-tests:make-test-session))
```

## Test Cases

### Valid Simple Evaluation

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2 3)"
    }
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "evaluate-lisp"
                                '(("code" . "(+ 1 2 3)")))))
  ;; Status: success
  (assert (result-response-p response))
  (assert (= (response-id response) 1))

  ;; Content structure
  (let ((content (result-field response :content)))
    (assert (= 1 (length content)))
    (assert (string= "text" (cdr (assoc :type (first content)))))
    (assert (string= "=> 6" (cdr (assoc :text (first content))))))

  ;; Not an error
  (assert (not (result-field response :isError))))
```

### Multiple Return Values

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(floor 17 5)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(floor 17 5)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 3" text))
  (assert (search "=> 2" text))
  (assert (not (result-field response :isError))))
```

### Package Context

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(package-name *package*)",
      "package": "KEYWORD"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(package-name *package*)")
                                   ("package" . "KEYWORD"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "KEYWORD" text)))
```

### Standard Output Capture

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(format t \"Hello, World!~%\") 42"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(format t \"Hello, World!~%\") 42"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[stdout]" text))
  (assert (search "Hello, World!" text))
  (assert (search "=> 42" text)))
```

### Error Output Capture

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(format *error-output* \"Warning!~%\") :ok"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(format *error-output* \"Warning!~%\") :ok"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[stderr]" text))
  (assert (search "Warning!" text))
  (assert (search "=> :OK" text)))
```

### Warning Capture

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(warn \"This is a warning\") :completed"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(warn \"This is a warning\") :completed"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[warnings]" text))
  (assert (search "WARNING" text))
  (assert (search "This is a warning" text))
  (assert (search "=> :COMPLETED" text))
  (assert (not (result-field response :isError))))
```

### Multiple Forms

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(setf x 10) (setf y 20) (+ x y)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(setf x 10) (setf y 20) (+ x y)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  ;; Only last form's result is returned
  (assert (search "=> 30" text))
  ;; Should not see intermediate values
  (assert (not (search "=> 10" text))))
```

### Error Response - Division by Zero

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(/ 1 0)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(/ 1 0)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (search "[ERROR]" text))
  (assert (search "DIVISION-BY-ZERO" text))
  (assert (search "[Backtrace]" text)))
```

### Error Response - Undefined Function

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(undefined-function 42)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(undefined-function 42)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (search "UNDEFINED-FUNCTION" text)))
```

### Error Response - Invalid Syntax

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(defun broken"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(defun broken"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (or (search "END-OF-FILE" text)
              (search "end of file" text))))
```

### Invalid Package Name

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2)",
      "package": "NONEXISTENT-PACKAGE"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(+ 1 2)")
                                   ("package" . "NONEXISTENT-PACKAGE"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (or (search "package" text)
              (search "PACKAGE" text))))
```

### Capture Timing Information

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(sleep 0.01) 42",
      "capture-time": true
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "(sleep 0.01) 42")
                                   ("capture-time" . t))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Timing:" text))
  (assert (search "ms real" text))
  (assert (search "ms run" text))
  (assert (search "bytes consed" text))
  (assert (search "=> 42" text)))
```

### Missing Required Parameter

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 13,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {}
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "evaluate-lisp" '())))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602)))  ; Invalid params
```

## Teardown

```lisp
(cl-mcp-server-tests:cleanup-test-session *test-session*)
```

## Notes

- Output sections are only present when non-empty
- Backtraces are truncated to relevant frames
- Timing information is optional and only shown when requested
- Package context defaults to CL-USER if not specified
- All streams are captured: stdout, stderr, warnings
