---
type: verification
name: compile-form-contract-test
source: contracts/compile-form-tool.md
level: contract
tags:
  - enhanced-evaluation
  - compilation
---

# Contract Test: Compile-Form Tool

## Purpose

Verify the `compile-form` tool correctly compiles code without execution, captures all compiler diagnostics, and maintains session state safety.

## Prerequisites

- Initialized MCP server with enhanced-evaluation tools
- Fresh session state for isolation

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)
```

## Test Cases

### Successful Compilation

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun double (x) (* x 2))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  ;; Success response
  (assert (result-response-p response))

  ;; Contains compilation statistics
  (assert (search "Compilation successful" (result-content response)))
  (assert (search "Warnings: 0" (result-content response)))
  (assert (search "Errors: 0" (result-content response)))
  (assert (search "Compiled 1 form" (result-content response)))

  ;; Function NOT defined in session
  (assert (not (fboundp 'double))))
```

### Compilation with Warning

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun uses-undefined () (nonexistent-function 42))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))

  ;; Warning captured
  (assert (search "WARNING" (result-content response)))
  (assert (search "Undefined function: NONEXISTENT-FUNCTION"
                  (result-content response) :test #'char-equal))
  (assert (search "Warnings: 1" (result-content response)))

  ;; Session unchanged
  (assert (not (fboundp 'uses-undefined))))
```

### Type Error Warning

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun bad-add (x) (+ x \"string\"))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))

  ;; Type warning present
  (assert (search "WARNING" (result-content response)))
  (assert (or (search "STRING" (result-content response))
              (search "not a NUMBER" (result-content response) :test #'char-equal))))
```

### Syntax Error

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun incomplete (x)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))

  ;; Compilation failed
  (assert (search "Compilation failed" (result-content response) :test #'char-equal))
  (assert (search "ERROR" (result-content response)))
  (assert (or (search "end of file" (result-content response) :test #'char-equal)
              (search "incomplete" (result-content response) :test #'char-equal))))
```

### No Execution Side Effects

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun side-effect () (print 'EXECUTED))"
  }
}
```

**Expected**:
```lisp
(let ((output (make-string-output-stream))
      (*standard-output* output))
  (let ((response (call-tool *server* "compile-form" input)))
    (assert (result-response-p response))

    ;; Nothing printed during compilation
    (assert (zerop (length (get-output-stream-string output))))

    ;; Function not defined
    (assert (not (fboundp 'side-effect)))))
```

### Package Context

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun test-func () (list 1 2 3))",
    "package": "CL-USER"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))
  (assert (search "Compilation successful" (result-content response))))
```

### Invalid Package

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun test () 42)",
    "package": "NONEXISTENT-PACKAGE"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (error-response-p response))
  (assert (search "package" (error-message response) :test #'char-equal)))
```

### Multiple Forms

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun foo () 1) (defun bar () 2) (defun baz () 3)"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))
  (assert (search "Compiled 3 forms" (result-content response)))

  ;; None of the functions defined
  (assert (not (fboundp 'foo)))
  (assert (not (fboundp 'bar)))
  (assert (not (fboundp 'baz))))
```

### Code with Macro

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun use-macro () (when t (+ 1 2)))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))
  (assert (search "Compilation successful" (result-content response))))
```

### Style Warning

**Input**:
```json
{
  "name": "compile-form",
  "arguments": {
    "code": "(defun unused-var (x) (let ((y 42)) x))"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "compile-form" input)))
  (assert (result-response-p response))
  ;; May contain style warning about unused variable Y
  ;; Implementation-specific, but should compile successfully
  (assert (search "Compil" (result-content response))))
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Notes

- All tests verify that compilation does NOT execute code
- Session state (function definitions, variables) must remain unchanged
- All compiler diagnostics must be captured and reported
- Package context must be respected
- Syntax errors must be caught and reported clearly
