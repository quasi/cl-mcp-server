# MCP Protocol Reference

**Complete reference for the Model Context Protocol (MCP) implementation in CL-MCP-Server.**

This document describes the wire protocol, message formats, and MCP-specific operations.

## Overview

CL-MCP-Server implements the Model Context Protocol (MCP) version `2025-03-26` over JSON-RPC 2.0 using stdio transport.

**Transport**: stdio (stdin for requests, stdout for responses)
**Encoding**: UTF-8
**Message Format**: JSON-RPC 2.0
**Protocol Version**: `2025-03-26`

## Message Format

All messages follow JSON-RPC 2.0 specification:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "method_name",
  "params": { ... }
}
```

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `jsonrpc` | string | Always `"2.0"` |
| `id` | number/string/null | Request identifier (required for requests, omitted for notifications) |
| `method` | string | Method name |
| `params` | object | Method parameters (optional) |

## Initialization Handshake

The initialization sequence establishes protocol compatibility and capabilities.

### 1. Initialize Request

**Method**: `initialize`

**Request**:
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

**Response**:
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

### 2. Initialized Notification

After receiving the initialize response, the client sends:

**Method**: `notifications/initialized`

**Message**:
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

**No response expected** (notifications don't receive responses).

### 3. Ready for Tool Calls

After initialization completes, the client can call tools.

## Tool Operations

### List Tools

**Method**: `tools/list`

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list"
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "evaluate-lisp",
        "description": "Evaluate Common Lisp code in a persistent session",
        "inputSchema": {
          "type": "object",
          "required": ["code"],
          "properties": {
            "code": {
              "type": "string",
              "description": "The Lisp code to evaluate"
            }
          }
        }
      }
    ]
  }
}
```

### Call Tool

**Method**: `tools/call`

**Request**:
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

**Response (Success)**:
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

**Response (Error)**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "[ERROR] DIVISION-BY-ZERO\nArithmetic error signalled.\n\n[Backtrace]\n0: (/ 1 0)\n..."
      }
    ],
    "isError": true
  }
}
```

## Server Capabilities

The server declares the following capabilities:

| Capability | Value | Description |
|------------|-------|-------------|
| `tools` | `{}` | Server provides tools (see `tools/list`) |
| `tools.listChanged` | Not set | Tool list is static (no dynamic changes) |

**Not Supported**:
- `resources` - No resource endpoints
- `prompts` - No prompt templates
- `sampling` - No LLM sampling

## Error Responses

### JSON-RPC Errors

For protocol-level errors, the server returns JSON-RPC error responses:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": "Missing required field: method"
  }
}
```

### Standard JSON-RPC Error Codes

| Code | Message | Meaning |
|------|---------|---------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid Request | Missing required fields |
| -32601 | Method not found | Unknown method |
| -32602 | Invalid params | Parameter validation failed |
| -32603 | Internal error | Server-side error |

### Application Errors

For evaluation errors (user code errors), the response uses `result` with `isError: true`:

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [{
      "type": "text",
      "text": "[ERROR] condition-type\nError message...\n[Backtrace]\n..."
    }],
    "isError": true
  }
}
```

**Note**: Evaluation errors are not protocol errors—they're successful tool executions that report a condition.

## Transport Details

### stdio Transport

**Input**: JSON-RPC messages on stdin, one per line
**Output**: JSON-RPC responses on stdout, one per line
**Logs**: Diagnostic output on stderr (not part of protocol)

### Message Framing

Messages are newline-delimited:

```
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}\n
{"jsonrpc":"2.0","id":1,"result":{...}}\n
```

### Encoding

All messages use UTF-8 encoding.

### Concurrency

The server processes messages sequentially:
- One request at a time
- Responses sent in order
- No parallel execution

## Protocol Invariants

These properties always hold:

**INV-001**: Every valid JSON-RPC request receives exactly one response
- Requests with `id` get responses with matching `id`
- Notifications (no `id`) get no response

**INV-005**: All messages conform to JSON-RPC 2.0
- Required fields present
- Valid JSON syntax
- Correct structure

**INV-002**: Server never terminates due to protocol errors
- Invalid requests get error responses
- Server remains operational

## Message Examples

### Complete Session

```
→ {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
← {"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-03-26","capabilities":{"tools":{}},"serverInfo":{"name":"cl-mcp-server","version":"0.1.0"}}}

→ {"jsonrpc":"2.0","method":"notifications/initialized"}

→ {"jsonrpc":"2.0","id":2,"method":"tools/list"}
← {"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"evaluate-lisp","description":"...","inputSchema":{...}}]}}

→ {"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(+ 1 2)"}}}
← {"jsonrpc":"2.0","id":3,"result":{"content":[{"type":"text","text":"=> 3"}],"isError":false}}
```

### Malformed Request

```
→ {"not-jsonrpc":"2.0","method":"initialize"}
← {"jsonrpc":"2.0","id":null,"error":{"code":-32600,"message":"Invalid Request","data":"Missing required field: jsonrpc"}}
```

### Unknown Method

```
→ {"jsonrpc":"2.0","id":1,"method":"unknown-method"}
← {"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"Method not found","data":"Method 'unknown-method' is not supported"}}
```

## Configuration

### Server Startup

Start the server with:

```bash
sbcl --load run-server.lisp
```

The server:
1. Loads the system
2. Starts the JSON-RPC server
3. Waits for messages on stdin
4. Sends responses to stdout

### Claude Code Configuration

Add to `~/.claude/mcp_config.json`:

```json
{
  "mcpServers": {
    "lisp": {
      "command": "sbcl",
      "args": [
        "--load", "/path/to/cl-mcp-server/run-server.lisp"
      ]
    }
  }
}
```

## Debugging

### Enable Verbose Logging

Server diagnostic output goes to stderr:

```bash
sbcl --load run-server.lisp 2>server.log
```

Check `server.log` for protocol-level debugging information.

### Test Messages Manually

Send messages directly:

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | sbcl --load run-server.lisp
```

### Validate JSON

Use `jq` to validate JSON before sending:

```bash
echo '{"jsonrpc":"2.0",...}' | jq '.'
```

## Protocol Conformance

CL-MCP-Server conforms to:

- [MCP Specification 2025-03-26](https://modelcontextprotocol.io/)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)

### Tested Clients

- Claude Code (tested)
- Any JSON-RPC 2.0 client (compatible)

## Limitations

- **No WebSocket transport**: Only stdio is supported
- **No bidirectional notifications**: Server doesn't initiate messages
- **No resource endpoints**: Only tools are provided
- **No prompts**: No prompt template system
- **Sequential processing**: One request at a time

## Future Protocol Enhancements

Planned improvements:

1. **Progress notifications**: Report long-running evaluations
2. **Cancellation**: Ability to cancel running evaluations
3. **Multiple sessions**: Isolated evaluation contexts
4. **Resource endpoints**: Expose Lisp image introspection

## See Also

- [evaluate-lisp Tool Reference](evaluate-lisp.md) - Primary tool documentation
- [Initialization Contract](../../canon/features/mcp-protocol/contracts/initialization.md) - Formal specification
- [Transport Contract](../../canon/features/mcp-protocol/contracts/transport.md) - stdio transport details
- [JSON-RPC 2.0 Spec](https://www.jsonrpc.org/specification) - Base protocol
- [MCP Documentation](https://modelcontextprotocol.io/) - Official MCP docs
