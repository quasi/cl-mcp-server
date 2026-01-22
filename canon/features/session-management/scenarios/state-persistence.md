---
type: scenario
name: state-persistence
feature: session-management
---

# Scenario: Session State Persistence

## Context

Claude defines things in one evaluation and uses them in later evaluations.

## Preconditions

- Server is initialized
- Session is fresh

## Test Cases

### Case 1: Function Persistence

**Request 1** (define):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defun greet (name) (format nil \"Hello, ~a!\" name))"}
  }
}
```

**Response 1**:
```json
{"jsonrpc": "2.0", "id": 1, "result": {"content": [{"type": "text", "text": "=> GREET"}], "isError": false}}
```

**Request 2** (use):
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(greet \"World\")"}
  }
}
```

**Response 2**:
```json
{"jsonrpc": "2.0", "id": 2, "result": {"content": [{"type": "text", "text": "=> \"Hello, World!\""}], "isError": false}}
```

### Case 2: Variable Accumulation

**Request 1**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defvar *items* '())"}
  }
}
```

**Request 2**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(push 'apple *items*)"}
  }
}
```

**Request 3**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(push 'banana *items*)"}
  }
}
```

**Request 4**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "*items*"}
  }
}
```

**Response 4**:
```json
{"jsonrpc": "2.0", "id": 6, "result": {"content": [{"type": "text", "text": "=> (BANANA APPLE)"}], "isError": false}}
```

### Case 3: Building on Previous Definitions

**Request 1** (base function):
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defun add (a b) (+ a b))"}
  }
}
```

**Request 2** (function using base):
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defun add-three (a b c) (add (add a b) c))"}
  }
}
```

**Request 3** (use combined):
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(add-three 1 2 3)"}
  }
}
```

**Response 3**:
```json
{"jsonrpc": "2.0", "id": 9, "result": {"content": [{"type": "text", "text": "=> 6"}], "isError": false}}
```

### Case 4: Package Persistence

**Request 1** (create and switch):
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defpackage :my-pkg (:use :cl)) (in-package :my-pkg)"}
  }
}
```

**Request 2** (define in new package):
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(defun internal-fn () :secret)"}
  }
}
```

**Request 3** (verify package context persisted):
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {"code": "(package-name *package*)"}
  }
}
```

**Response 3**:
```json
{"jsonrpc": "2.0", "id": 12, "result": {"content": [{"type": "text", "text": "=> \"MY-PKG\""}], "isError": false}}
```

## Invariants Verified

- INV-003: Session State Persistence
