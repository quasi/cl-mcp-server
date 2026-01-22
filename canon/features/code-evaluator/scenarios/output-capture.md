---
type: scenario
name: output-capture
feature: code-evaluator
---

# Scenario: Output Stream Capture

## Context

Claude evaluates code that produces output to stdout/stderr.

## Preconditions

- Server is initialized

## Test Cases

### Case 1: Standard Output

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(format t \"Hello, World!~%\") 42"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{"type": "text", "text": "[stdout]\nHello, World!\n\n=> 42"}],
    "isError": false
  }
}
```

### Case 2: Multiple Print Statements

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(print 1) (print 2) (print 3)"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "content": [{"type": "text", "text": "[stdout]\n\n1 \n2 \n3 \n=> 3"}],
    "isError": false
  }
}
```

### Case 3: Error Output

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(format *error-output* \"Warning: something happened~%\") :ok"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [{"type": "text", "text": "[stderr]\nWarning: something happened\n\n=> :OK"}],
    "isError": false
  }
}
```

### Case 4: Mixed Output

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(format t \"stdout line~%\") (format *error-output* \"stderr line~%\") :done"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [{"type": "text", "text": "[stdout]\nstdout line\n\n[stderr]\nstderr line\n\n=> :DONE"}],
    "isError": false
  }
}
```

### Case 5: Warning Capture

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(warn \"This is a warning\") :completed"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "content": [{"type": "text", "text": "[warnings]\nWARNING: This is a warning\n\n=> :COMPLETED"}],
    "isError": false
  }
}
```

### Case 6: No Output (Clean Result)

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(* 6 7)"
    }
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [{"type": "text", "text": "=> 42"}],
    "isError": false
  }
}
```

Note: No `[stdout]` or `[stderr]` sections when there's no output.

## Invariants Verified

- INV-004: Output Stream Separation (stdout, stderr, warnings, values all distinguishable)
