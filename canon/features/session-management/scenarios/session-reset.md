---
type: scenario
name: session-reset
feature: session-management
---

# Scenario: Session Reset

## Context

Claude wants to start fresh, clearing all previous definitions.

## Preconditions

- Server is initialized
- Session has accumulated state (functions, variables)

## Setup

First, create some state:

**Setup Request 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defun my-fn () 42) (defvar *my-var* 100)"}
  }
}
```

**Setup Request 2** (verify state exists):
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "list-definitions",
    "arguments": {}
  }
}
```

**Setup Response 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "content": [{"type": "text", "text": "[Functions]\n- MY-FN ()\n\n[Variables]\n- *MY-VAR* = 100"}],
    "isError": false
  }
}
```

## Test: Reset Session

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "reset-session",
    "arguments": {}
  }
}
```

**Expected Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [{"type": "text", "text": "Session reset. All definitions cleared.\nCurrent package: CL-USER"}],
    "isError": false
  }
}
```

## Verification: State is Gone

**Request 1** (list definitions):
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "list-definitions",
    "arguments": {}
  }
}
```

**Expected Response 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [{"type": "text", "text": "No user definitions."}],
    "isError": false
  }
}
```

**Request 2** (try to use cleared function):
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(my-fn)"}
  }
}
```

**Expected Response 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "content": [{"type": "text", "text": "[ERROR] UNDEFINED-FUNCTION\nThe function MY-FN is undefined.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

**Request 3** (try to use cleared variable):
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "*my-var*"}
  }
}
```

**Expected Response 3**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [{"type": "text", "text": "[ERROR] UNBOUND-VARIABLE\nThe variable *MY-VAR* is unbound.\n\n[Backtrace]\n..."}],
    "isError": true
  }
}
```

## Postconditions

- All user definitions are cleared
- Current package is CL-USER
- Session is ready for new definitions
- Loaded systems (if any) remain available
