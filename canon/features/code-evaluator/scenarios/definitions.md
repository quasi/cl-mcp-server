---
type: scenario
name: definitions
feature: code-evaluator
---

# Scenario: Function and Variable Definitions

## Context

Claude defines functions and variables that persist across evaluations.

## Preconditions

- Server is initialized
- Session is fresh (or reset)

## Test Cases

### Case 1: Define a Function

**Request 1** (define):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(defun square (x) (* x x))"
    }
  }
}
```

**Expected Response 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": [{"type": "text", "text": "=> SQUARE"}],
    "isError": false
  }
}
```

**Request 2** (use):
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(square 5)"
    }
  }
}
```

**Expected Response 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "content": [{"type": "text", "text": "=> 25"}],
    "isError": false
  }
}
```

### Case 2: Define a Variable

**Request 1** (define):
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(defvar *counter* 0)"
    }
  }
}
```

**Expected Response 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [{"type": "text", "text": "=> *COUNTER*"}],
    "isError": false
  }
}
```

**Request 2** (use and modify):
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(incf *counter*) (incf *counter*) *counter*"
    }
  }
}
```

**Expected Response 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [{"type": "text", "text": "=> 2"}],
    "isError": false
  }
}
```

### Case 3: Define a Macro

**Request 1** (define):
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(defmacro with-timing (&body body) `(let ((start (get-internal-real-time))) (prog1 (progn ,@body) (format t \"Time: ~,3f seconds~%\" (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))"
    }
  }
}
```

**Expected Response 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "content": [{"type": "text", "text": "=> WITH-TIMING"}],
    "isError": false
  }
}
```

**Request 2** (use):
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(with-timing (sleep 0.1))"
    }
  }
}
```

**Expected Response 2** (approximate):
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [{"type": "text", "text": "[stdout]\nTime: 0.100 seconds\n\n=> NIL"}],
    "isError": false
  }
}
```

### Case 4: Define a Class

**Request 1** (define):
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(defclass point () ((x :initarg :x :accessor point-x) (y :initarg :y :accessor point-y)))"
    }
  }
}
```

**Expected Response 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "result": {
    "content": [{"type": "text", "text": "=> #<STANDARD-CLASS POINT>"}],
    "isError": false
  }
}
```

**Request 2** (use):
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(let ((p (make-instance 'point :x 3 :y 4))) (sqrt (+ (expt (point-x p) 2) (expt (point-y p) 2))))"
    }
  }
}
```

**Expected Response 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "content": [{"type": "text", "text": "=> 5.0"}],
    "isError": false
  }
}
```

## Postconditions

- All definitions remain available for subsequent evaluations
- Session state includes: SQUARE, *COUNTER*, WITH-TIMING, POINT

## Invariants Verified

- INV-003: Session State Persistence
