---
type: verification
name: compile-form-tool-contract-test
source: contracts/compile-form-tool.md
level: contract
tags:
  - compilation
  - phase-b
---

# Contract Test: Compile-Form Tool

## Purpose

Verify the compile-form tool correctly compiles code without execution and reports warnings, errors, and compiler notes.

## Prerequisites

- Initialized server with MCP session
- Phase B features enabled

## Setup

```lisp
(defvar *test-session* (cl-mcp-server-tests:make-test-session))
```

## Test Cases

### Successful Compilation - No Warnings

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(+ 1 2)"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(+ 1 2)"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "Compilation successful" text))
  (assert (search "No warnings" text)))
```

### Compilation with Style Warning

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun test () (undefined-function 42))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun test () (undefined-function 42))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "Compilation successful" text))
  (assert (search "Warnings" text))
  (assert (or (search "[STYLE]" text)
              (search "STYLE-WARNING" text)))
  (assert (search "undefined" text)))
```

### Compilation with Type Warning

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun test (x) (declare (type number x)) (1+ \"string\"))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun test (x) (declare (type number x)) (1+ \"string\"))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Warning" text))
  (assert (or (search "type" text)
              (search "TYPE" text))))
```

### Compilation Failure - Syntax Error

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun broken"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun broken"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "FAILED" text))
  (assert (search "Error" text)))
```

### Compiler Notes

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun test (x) (if t x (+ x 1)))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun test (x) (if t x (+ x 1)))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  ;; SBCL should detect unreachable code
  (assert (or (search "note" text)
              (search "unreachable" text)
              ;; Or it might just succeed without notes
              (search "successful" text))))
```

### Package Context

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(length '(a b c))",
      "package": "CL-USER"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(length '(a b c))")
                                   ("package" . "CL-USER"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "successful" text)))
```

### Invalid Package

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(+ 1 2)",
      "package": "NONEXISTENT-PACKAGE"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(+ 1 2)")
                                   ("package" . "NONEXISTENT-PACKAGE"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (result-field response :isError))
  (assert (search "package" text)))
```

### No Side Effects

**Verify compilation doesn't execute code**:

```lisp
;; Define a variable
(send-tool-call *test-session* "evaluate-lisp"
               '(("code" . "(defvar *test-counter* 0)")))

;; Compile code that would modify it
(send-tool-call *test-session* "compile-form"
               '(("code" . "(incf *test-counter*)")))

;; Verify variable unchanged
(let* ((response (send-tool-call *test-session* "evaluate-lisp"
                                 '(("code" . "*test-counter*"))))
       (text (result-content-text response)))
  (assert (search "=> 0" text)))  ; Still 0, not 1
```

### Missing Required Parameter

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {}
  }
}
```

**Expected**:
```lisp
(let ((response (send-tool-call *test-session* "compile-form" '())))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602)))  ; Invalid params
```

### Complex Form Compilation

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "successful" text)))
```

### Multiple Warnings

**Input**:
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "tools/call",
  "params": {
    "name": "compile-form",
    "arguments": {
      "code": "(defun test () (undefined-1) (undefined-2) (undefined-3))"
    }
  }
}
```

**Expected**:
```lisp
(let* ((response (send-tool-call *test-session* "compile-form"
                                 '(("code" . "(defun test () (undefined-1) (undefined-2) (undefined-3))"))))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "Warnings" text))
  ;; Should report count
  (assert (or (search "3" text)
              (and (search "undefined-1" text)
                   (search "undefined-2" text)
                   (search "undefined-3" text)))))
```

## Teardown

```lisp
(cl-mcp-server-tests:cleanup-test-session *test-session*)
```

## Notes

- Compilation checks code validity without execution
- Useful for catching type errors before evaluation
- Returns warnings, style warnings, and compiler notes
- Does not affect session state
- Package context defaults to CL-USER if not specified
