---
type: contract
name: initialization
version: 0.1.0
---

# MCP Initialization Contract

Defines the initialization handshake between client and server.

## Protocol Version

The server implements MCP protocol version `2025-03-26` (stable).

## Initialize Request

**Method**: `initialize`

**Input Schema**:
```json
{
  "type": "object",
  "required": ["protocolVersion", "capabilities", "clientInfo"],
  "properties": {
    "protocolVersion": {
      "type": "string",
      "description": "Protocol version the client supports"
    },
    "capabilities": {
      "type": "object",
      "description": "Client capabilities"
    },
    "clientInfo": {
      "type": "object",
      "required": ["name", "version"],
      "properties": {
        "name": {"type": "string"},
        "version": {"type": "string"}
      }
    }
  }
}
```

**Example Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-03-26",
    "capabilities": {},
    "clientInfo": {
      "name": "claude-code",
      "version": "1.0.0"
    }
  }
}
```

## Initialize Response

**Output Schema**:
```json
{
  "type": "object",
  "required": ["protocolVersion", "capabilities", "serverInfo"],
  "properties": {
    "protocolVersion": {
      "type": "string",
      "description": "Protocol version the server supports"
    },
    "capabilities": {
      "type": "object",
      "properties": {
        "tools": {
          "type": "object",
          "properties": {
            "listChanged": {"type": "boolean"}
          }
        }
      }
    },
    "serverInfo": {
      "type": "object",
      "required": ["name", "version"],
      "properties": {
        "name": {"type": "string"},
        "version": {"type": "string"}
      }
    }
  }
}
```

**Example Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-03-26",
    "capabilities": {
      "tools": {}
    },
    "serverInfo": {
      "name": "cl-mcp-server",
      "version": "0.1.0"
    }
  }
}
```

## Initialized Notification

After receiving the initialize response, the client sends a notification:

**Method**: `notifications/initialized`

**Example**:
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

No response is expected for notifications.

## Server Capabilities

The CL-MCP-Server declares:

| Capability | Value | Description |
|------------|-------|-------------|
| `tools` | `{}` | Server provides tools |
| `tools.listChanged` | `false` | Tool list is static |

## Error Response

If initialization fails:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": "Missing required field: protocolVersion"
  }
}
```

### Possible Errors

| Code | Message | When |
|------|---------|------|
| -32600 | Invalid Request | Missing required fields |
| -32602 | Invalid params | Malformed capabilities or clientInfo |
| -32000 | Protocol version mismatch | Incompatible protocol version |

### Protocol Version Mismatch

If client requests an unsupported protocol version:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32000,
    "message": "Unsupported protocol version",
    "data": {
      "requested": "2024-01-01",
      "supported": ["2025-03-26"]
    }
  }
}
```

## Invariants

1. Server MUST NOT process any requests before `initialize` completes
2. Server MUST respond to `initialize` with compatible protocol version
3. Client MUST send `notifications/initialized` before calling tools
