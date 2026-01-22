---
type: scenario
name: evaluation-errors
feature: error-handling
---

# Scenario: Evaluation Error Handling

## Context

Claude sends code that triggers various runtime errors.

## Preconditions

- Server is initialized

## Test Cases

### Case 1: Undefined Function

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(nonexistent-function 1 2 3)"
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
    "content": [{"type": "text", "text": "[ERROR] UNDEFINED-FUNCTION\nThe function NONEXISTENT-FUNCTION is undefined.\n\n[Backtrace]\n0: (NONEXISTENT-FUNCTION 1 2 3)\n..."}],
    "isError": true
  }
}
```

### Case 2: Unbound Variable

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ x 1)"
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
    "content": [{"type": "text", "text": "[ERROR] UNBOUND-VARIABLE\nThe variable X is unbound.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

### Case 3: Type Error

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 \"hello\")"
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
    "content": [{"type": "text", "text": "[ERROR] TYPE-ERROR\nThe value \"hello\" is not of type NUMBER.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

### Case 4: Division by Zero

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(/ 1 0)"
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
    "content": [{"type": "text", "text": "[ERROR] DIVISION-BY-ZERO\nArithmetic error DIVISION-BY-ZERO signalled.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

### Case 5: Index Out of Bounds

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(aref #(1 2 3) 10)"
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
    "content": [{"type": "text", "text": "[ERROR] SB-KERNEL:INDEX-TOO-LARGE-ERROR\nInvalid index 10 for (SIMPLE-VECTOR 3).\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

### Case 6: Reader Error (Syntax)

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2"
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
    "content": [{"type": "text", "text": "[ERROR] END-OF-FILE\nUnexpected end of file while reading.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

## Postconditions

- Server remains operational after each error
- Session state is preserved (errors don't corrupt session)
- Subsequent evaluations work normally

## Invariants Verified

- INV-002: Server Stability (server doesn't crash)
- INV-006: Condition Type Preservation (type is reported, not just message)
