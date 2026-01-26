---
type: contract
name: shared-types
version: 0.1.0
---

# Shared Types

Common data structures used across CL-MCP-Server features.

---

## JSON-RPC Types

### JsonRpcRequest

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/JsonRpcRequest",
  "type": "object",
  "required": ["jsonrpc", "method"],
  "properties": {
    "jsonrpc": {
      "type": "string",
      "const": "2.0"
    },
    "id": {
      "oneOf": [
        {"type": "string"},
        {"type": "integer"},
        {"type": "null"}
      ],
      "description": "Request identifier. Null or absent for notifications."
    },
    "method": {
      "type": "string",
      "description": "Method name to invoke"
    },
    "params": {
      "oneOf": [
        {"type": "object"},
        {"type": "array"}
      ],
      "description": "Method parameters"
    }
  }
}
```

### JsonRpcResponse

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/JsonRpcResponse",
  "type": "object",
  "required": ["jsonrpc", "id"],
  "oneOf": [
    {
      "required": ["result"],
      "properties": {
        "jsonrpc": {"const": "2.0"},
        "id": {"$ref": "#/definitions/JsonRpcRequest/properties/id"},
        "result": {}
      }
    },
    {
      "required": ["error"],
      "properties": {
        "jsonrpc": {"const": "2.0"},
        "id": {"$ref": "#/definitions/JsonRpcRequest/properties/id"},
        "error": {"$ref": "#/definitions/JsonRpcError"}
      }
    }
  ]
}
```

### JsonRpcError

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/JsonRpcError",
  "type": "object",
  "required": ["code", "message"],
  "properties": {
    "code": {
      "type": "integer",
      "description": "Error code per JSON-RPC 2.0 spec"
    },
    "message": {
      "type": "string",
      "description": "Short error description"
    },
    "data": {
      "description": "Additional error information"
    }
  }
}
```

---

## MCP Types

### ToolDefinition

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/ToolDefinition",
  "type": "object",
  "required": ["name", "description", "inputSchema"],
  "properties": {
    "name": {
      "type": "string",
      "description": "Tool identifier"
    },
    "description": {
      "type": "string",
      "description": "Human-readable tool description"
    },
    "inputSchema": {
      "type": "object",
      "description": "JSON Schema for tool parameters"
    }
  }
}
```

---

## Evaluation Types

### EvaluationResult

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/EvaluationResult",
  "type": "object",
  "required": ["success"],
  "properties": {
    "success": {
      "type": "boolean",
      "description": "Whether evaluation completed without error"
    },
    "values": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Printed representations of return values"
    },
    "stdout": {
      "type": "string",
      "description": "Captured standard output"
    },
    "stderr": {
      "type": "string",
      "description": "Captured standard error"
    },
    "error": {
      "$ref": "#/definitions/LispCondition"
    },
    "warnings": {
      "type": "array",
      "items": {"$ref": "#/definitions/LispCondition"},
      "description": "Warnings signaled during evaluation"
    }
  }
}
```

### LispCondition

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/LispCondition",
  "type": "object",
  "required": ["type", "message"],
  "properties": {
    "type": {
      "type": "string",
      "description": "Condition class name (e.g., 'SIMPLE-ERROR', 'TYPE-ERROR')"
    },
    "message": {
      "type": "string",
      "description": "Condition message text"
    },
    "backtrace": {
      "type": "string",
      "description": "Formatted stack trace"
    }
  }
}
```

---

## Error Response Format

All tools that can fail use this standard error response format within MCP:

### Tool Error Response

```json-schema
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "#/definitions/ToolErrorResponse",
  "type": "object",
  "required": ["content", "isError"],
  "properties": {
    "content": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["type", "text"],
        "properties": {
          "type": {"const": "text"},
          "text": {
            "type": "string",
            "description": "Error message, typically formatted as '[ERROR] TYPE\\nmessage'"
          }
        }
      }
    },
    "isError": {
      "type": "boolean",
      "const": true
    }
  }
}
```

### Common Condition Types

| Condition | Description |
|-----------|-------------|
| `simple-error` | General error |
| `type-error` | Type mismatch |
| `package-error` | Package not found |
| `reader-error` | Malformed Lisp syntax |
| `end-of-file` | Incomplete form |
| `sb-ext:timeout` | Operation timed out |
| `storage-condition` | Out of memory |
