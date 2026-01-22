---
type: contract
name: tools-list
version: 0.1.0
---

# Tools List Contract

Defines how the server advertises available tools.

## Request

**Method**: `tools/list`

**Input Schema**:
```json
{
  "type": "object",
  "properties": {
    "cursor": {
      "type": "string",
      "description": "Optional pagination cursor (not used by this server)"
    }
  }
}
```

**Example Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list"
}
```

## Response

**Output Schema**:
```json
{
  "type": "object",
  "required": ["tools"],
  "properties": {
    "tools": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["name", "description", "inputSchema"],
        "properties": {
          "name": {"type": "string"},
          "description": {"type": "string"},
          "inputSchema": {"type": "object"}
        }
      }
    }
  }
}
```

## Exposed Tools

The CL-MCP-Server exposes these tools:

### 1. evaluate-lisp

Evaluates Common Lisp code in the persistent session.

```json
{
  "name": "evaluate-lisp",
  "description": "Evaluate Common Lisp code in a persistent REPL session. Definitions and variables persist across calls.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp expression(s) to evaluate"
      },
      "package": {
        "type": "string",
        "description": "Package context for evaluation (default: CL-USER)"
      }
    }
  }
}
```

### 2. list-definitions

Lists definitions in the current session.

```json
{
  "name": "list-definitions",
  "description": "List functions, variables, and other definitions in the current session.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "type": {
        "type": "string",
        "enum": ["all", "functions", "variables", "macros", "classes"],
        "description": "Filter by definition type (default: all)"
      }
    }
  }
}
```

### 3. reset-session

Resets the evaluation session to initial state.

```json
{
  "name": "reset-session",
  "description": "Clear all session state including definitions and variables. Start fresh.",
  "inputSchema": {
    "type": "object",
    "properties": {}
  }
}
```

### 4. load-system

Loads an ASDF system via Quicklisp.

```json
{
  "name": "load-system",
  "description": "Load an ASDF system using Quicklisp. The system becomes available for subsequent evaluations.",
  "inputSchema": {
    "type": "object",
    "required": ["system"],
    "properties": {
      "system": {
        "type": "string",
        "description": "ASDF system name to load (e.g., 'alexandria', 'cl-ppcre')"
      }
    }
  }
}
```

## Example Response

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "evaluate-lisp",
        "description": "Evaluate Common Lisp code in a persistent REPL session. Definitions and variables persist across calls.",
        "inputSchema": {
          "type": "object",
          "required": ["code"],
          "properties": {
            "code": {
              "type": "string",
              "description": "Common Lisp expression(s) to evaluate"
            },
            "package": {
              "type": "string",
              "description": "Package context for evaluation (default: CL-USER)"
            }
          }
        }
      },
      {
        "name": "list-definitions",
        "description": "List functions, variables, and other definitions in the current session.",
        "inputSchema": {
          "type": "object",
          "properties": {
            "type": {
              "type": "string",
              "enum": ["all", "functions", "variables", "macros", "classes"],
              "description": "Filter by definition type (default: all)"
            }
          }
        }
      },
      {
        "name": "reset-session",
        "description": "Clear all session state including definitions and variables. Start fresh.",
        "inputSchema": {
          "type": "object",
          "properties": {}
        }
      },
      {
        "name": "load-system",
        "description": "Load an ASDF system using Quicklisp. The system becomes available for subsequent evaluations.",
        "inputSchema": {
          "type": "object",
          "required": ["system"],
          "properties": {
            "system": {
              "type": "string",
              "description": "ASDF system name to load"
            }
          }
        }
      }
    ]
  }
}
```
