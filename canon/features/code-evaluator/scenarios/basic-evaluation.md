---
type: scenario
name: basic-evaluation
feature: code-evaluator
---

# Scenario: Basic Code Evaluation

## Context

Claude sends simple Lisp expressions for evaluation.

## Preconditions

- Server is initialized
- Session is in default state (CL-USER package)

## Test Cases

### Case 1: Arithmetic Expression

**Request**:
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

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{"type": "text", "text": "=> 6"}],
    "isError": false
  }
}
```

### Case 2: String Operations

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(concatenate 'string \"Hello, \" \"World!\")"
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
    "content": [{"type": "text", "text": "=> \"Hello, World!\""}],
    "isError": false
  }
}
```

### Case 3: List Operations

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(mapcar #'1+ '(1 2 3 4 5))"
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
    "content": [{"type": "text", "text": "=> (2 3 4 5 6)"}],
    "isError": false
  }
}
```

### Case 4: Multiple Values

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(floor 17 5)"
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
    "content": [{"type": "text", "text": "=> 3\n=> 2"}],
    "isError": false
  }
}
```

### Case 5: NIL Result

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(member 'x '(a b c))"
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
    "content": [{"type": "text", "text": "=> NIL"}],
    "isError": false
  }
}
```

### Case 6: Multiple Forms

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(setf x 10) (setf y 20) (+ x y)"
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
    "content": [{"type": "text", "text": "=> 30"}],
    "isError": false
  }
}
```

Note: Only the last form's result is returned.

## Postconditions

- Session state may have changed (variables set in Case 6)
- No side effects outside the Lisp image
