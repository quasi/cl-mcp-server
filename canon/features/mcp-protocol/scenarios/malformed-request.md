---
type: scenario
name: malformed-request
feature: mcp-protocol
---

# Scenario: Malformed Request Handling

## Context

The server receives invalid JSON or invalid JSON-RPC requests and must respond with appropriate errors without crashing.

## Preconditions

- Server is initialized and ready

## Test Cases

### Case 1: Invalid JSON

**Input** (stdin):
```
{not valid json}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":null,"error":{"code":-32700,"message":"Parse error"}}
```

### Case 2: Missing jsonrpc field

**Input** (stdin):
```json
{"id":1,"method":"tools/list"}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Invalid Request: missing jsonrpc field"}}
```

### Case 3: Unknown method

**Input** (stdin):
```json
{"jsonrpc":"2.0","id":1,"method":"unknown/method"}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"Method not found: unknown/method"}}
```

### Case 4: Invalid tool name

**Input** (stdin):
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"nonexistent-tool","arguments":{}}}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"Unknown tool: nonexistent-tool"}}
```

### Case 5: Missing required parameter

**Input** (stdin):
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{}}}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"Invalid params: missing required parameter 'code'"}}
```

## Postconditions

- Server remains operational after each error
- Server continues to accept new requests

## Invariants Verified

- INV-001: Request-Response Guarantee (errors still get responses)
- INV-002: Server Stability (server doesn't crash on bad input)
- INV-005: Protocol Conformance (error responses are valid JSON-RPC)
