---
type: contract
name: tools-call
version: 0.1.0
---

# Tools Call Contract

Defines how clients invoke tools and receive results.

## Request

**Method**: `tools/call`

**Input Schema**:
```json
{
  "type": "object",
  "required": ["name", "arguments"],
  "properties": {
    "name": {
      "type": "string",
      "description": "Name of the tool to invoke"
    },
    "arguments": {
      "type": "object",
      "description": "Tool-specific arguments"
    }
  }
}
```

**Example Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "evaluate-lisp",
    "arguments": {
      "code": "(+ 1 2 3)"
    }
  }
}
```

## Response

**Output Schema**:
```json
{
  "type": "object",
  "required": ["content"],
  "properties": {
    "content": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["type"],
        "properties": {
          "type": {
            "type": "string",
            "enum": ["text"]
          },
          "text": {
            "type": "string"
          }
        }
      }
    },
    "isError": {
      "type": "boolean",
      "description": "True if tool execution failed"
    }
  }
}
```

## Success Response Format

For successful evaluations, the response includes:

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "=> 6"
      }
    ],
    "isError": false
  }
}
```

### Multi-Value Results

When the expression returns multiple values:

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "=> 3\n=> 1"
      }
    ],
    "isError": false
  }
}
```

### Results with Output

When the expression produces stdout/stderr output:

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "[stdout]\nHello, World!\n\n[stderr]\nWarning: deprecated function\n\n=> NIL"
      }
    ],
    "isError": false
  }
}
```

## Error Response Format

### Tool Execution Errors

When evaluation signals a condition:

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "[ERROR] UNDEFINED-FUNCTION\nThe function NONEXISTENT-FUNCTION is undefined.\n\n[Backtrace]\n0: (NONEXISTENT-FUNCTION)\n1: (EVAL (NONEXISTENT-FUNCTION))\n..."
      }
    ],
    "isError": true
  }
}
```

### Protocol Errors

For invalid requests (wrong tool name, invalid arguments):

```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "error": {
    "code": -32602,
    "message": "Unknown tool: invalid-tool-name"
  }
}
```

## Tool-Specific Responses

### evaluate-lisp

Returns evaluation result with optional output sections:

```
[stdout]
{captured standard output}

[stderr]
{captured error output}

[warnings]
{any warnings signaled}

=> {primary value}
=> {secondary value}
...
```

### list-definitions

Returns structured listing:

```
[Functions]
- MY-FUNCTION (A B &OPTIONAL C)
- HELPER (X)

[Variables]
- *MY-VAR* = 42
- +MY-CONSTANT+ = "hello"

[Macros]
- WITH-TIMING (FORM)
```

### reset-session

Returns confirmation:

```
Session reset. All definitions cleared.
```

### load-system

Returns loading status:

```
Loading system: alexandria
Loaded: alexandria (version 1.0.0)
```

Or on failure:

```
[ERROR] QUICKLISP-CLIENT:SYSTEM-NOT-FOUND
System "nonexistent-system" not found.
```
