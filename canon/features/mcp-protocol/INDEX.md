# MCP Protocol Feature Index

**Purpose**: JSON-RPC 2.0 communication over stdio implementing the Model Context Protocol.

**Status**: draft
**Priority**: P0 (foundation layer)

## Quick Navigation

| Need to... | Read |
|------------|------|
| Implement initialization handshake | `contracts/initialization.md` → `scenarios/initialization-handshake.md` |
| List available tools | `contracts/tools-list.md` |
| Handle tool invocation | `contracts/tools-call.md` |
| Understand stdio transport | `contracts/transport.md` |
| Debug protocol errors | `scenarios/malformed-request.md` |

## Contents

### Contracts (4)

| File | Purpose | Lines | Key Points |
|------|---------|-------|------------|
| `contracts/initialization.md` | MCP handshake | ~145 | Protocol version 2025-03-26, server capabilities |
| `contracts/tools-list.md` | Tool enumeration | TBD | Returns `evaluate-lisp` tool definition |
| `contracts/tools-call.md` | Tool invocation | TBD | Dispatches to evaluate-lisp implementation |
| `contracts/transport.md` | stdio communication | TBD | stdin=requests, stdout=responses |

### Scenarios (2)

| File | Purpose | Test Cases |
|------|---------|------------|
| `scenarios/initialization-handshake.md` | Startup flow | Full handshake: initialize → response → initialized notification |
| `scenarios/malformed-request.md` | Error handling | Invalid JSON, missing fields, unknown methods |

### Vocabulary (1)

`vocabulary.md` - MCP-specific terms:
- **JSON-RPC**: Request-response protocol
- **Method**: Operation identifier
- **Notification**: One-way message (no response)
- **Capabilities**: Feature advertisement

## Protocol Overview

### Communication Pattern

```
Claude ──stdin──> MCP Server
Claude <─stdout── MCP Server
```

### Message Flow

```
1. Claude → initialize (request)
2. MCP Server → initialize (response with capabilities)
3. Claude → notifications/initialized (notification)
4. Claude ↔ MCP Server (tools/list, tools/call requests)
```

### Required Methods

| Method | Type | Purpose | Response |
|--------|------|---------|----------|
| `initialize` | Request | Handshake | Server info & capabilities |
| `notifications/initialized` | Notification | Client ready | None |
| `tools/list` | Request | Enumerate tools | Tool definitions |
| `tools/call` | Request | Execute tool | Tool result |

## JSON-RPC 2.0 Structure

### Request Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "method-name",
  "params": { /* method-specific */ }
}
```

### Response Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": { /* method-specific */ }
}
```

### Error Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Invalid Request",
    "data": { /* optional */ }
  }
}
```

### Notification Format

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

**Key**: Notifications omit `id` field.

## Server Capabilities

This server declares:

```json
{
  "capabilities": {
    "tools": {}
  }
}
```

**Meaning**:
- Server provides tools (via `tools/list` and `tools/call`)
- Tool list is static (not `tools.listChanged`)

## Implementation Notes

### stdio Transport

**Key constraints**:
- **stdin**: Line-delimited JSON requests
- **stdout**: Line-delimited JSON responses
- **stderr**: NOT used for MCP (reserved for debugging/logging)

**Important**: Each JSON message must be on a single line (no pretty-printing).

### Error Codes

Standard JSON-RPC 2.0 error codes:

| Code | Meaning | When to Use |
|------|---------|-------------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid Request | Missing required fields |
| -32601 | Method not found | Unknown method |
| -32602 | Invalid params | Bad parameter types |
| -32603 | Internal error | Server error |

### Initialization Requirements

From `contracts/initialization.md` invariants:

1. Server MUST NOT process requests before `initialize` completes
2. Server MUST respond with compatible protocol version
3. Client MUST send `notifications/initialized` before calling tools

## Dependencies

**Depends on**:
- `core/foundation/vocabulary.md` (MCP Server, Transport, Tool)
- `core/foundation/invariants.md` (INV-001: request-response guarantee, INV-005: protocol conformance)

**Depended on by**:
- `code-evaluator` (uses tool interface)
- `error-handling` (uses error format)
- `session-management` (uses notification pattern)

## Verification

See `verification/` directory for:
- Protocol conformance tests
- Message schema validation
- Handshake sequence verification

## Common Issues

### Issue: Response not received

**Symptom**: Claude hangs waiting for response.

**Cause**: Violated INV-001 (request-response guarantee).

**Solution**: Ensure every request gets exactly one response, even on error.

### Issue: Malformed JSON

**Symptom**: Parse errors in Claude.

**Cause**: Pretty-printed JSON or missing newlines.

**Solution**: Output single-line JSON with trailing newline.

### Issue: Tools called before initialization

**Symptom**: Unexpected behavior or errors.

**Cause**: Processing `tools/call` before `initialize` completes.

**Solution**: Track initialization state, reject requests if not initialized.

---

**Last Updated**: 2026-01-22
**Status**: draft
**Next Steps**: Implement remaining contracts (tools-list, tools-call, transport)
