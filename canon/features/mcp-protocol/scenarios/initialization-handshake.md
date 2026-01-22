---
type: scenario
name: initialization-handshake
feature: mcp-protocol
---

# Scenario: Initialization Handshake

## Context

Claude Code launches the CL-MCP-Server and needs to establish a connection before using any tools.

## Preconditions

- Server process has been spawned
- Server is listening on stdin

## Steps

### Step 1: Client sends initialize request

**Input** (stdin):
```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"claude-code","version":"1.0.0"}}}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-03-26","capabilities":{"tools":{}},"serverInfo":{"name":"cl-mcp-server","version":"0.1.0"}}}
```

### Step 2: Client sends initialized notification

**Input** (stdin):
```json
{"jsonrpc":"2.0","method":"notifications/initialized"}
```

**Expected Output**: None (notifications don't receive responses)

### Step 3: Client discovers available tools

**Input** (stdin):
```json
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
```

**Expected Output** (stdout):
```json
{"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"evaluate-lisp","description":"Evaluate Common Lisp code in a persistent REPL session. Definitions and variables persist across calls.","inputSchema":{"type":"object","required":["code"],"properties":{"code":{"type":"string","description":"Common Lisp expression(s) to evaluate"},"package":{"type":"string","description":"Package context for evaluation (default: CL-USER)"}}}}]}}
```

## Postconditions

- Server is ready to accept `tools/call` requests
- Client knows available tools and their schemas

## Invariants Verified

- INV-001: Request-Response Guarantee (every request got a response)
- INV-005: Protocol Conformance (all messages are valid JSON-RPC 2.0)
