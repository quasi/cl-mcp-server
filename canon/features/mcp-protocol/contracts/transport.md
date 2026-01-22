---
type: contract
name: transport
version: 0.1.0
---

# Stdio Transport Contract

Defines the communication channel between client and server.

## Transport Mechanism

The server uses **stdio transport**:
- Reads JSON-RPC messages from `stdin`
- Writes JSON-RPC messages to `stdout`
- Logging/debug output goes to `stderr`

## Message Framing

Messages use **Newline-Delimited JSON (NDJSON)**:

```
{"jsonrpc":"2.0","id":1,"method":"initialize",...}\n
{"jsonrpc":"2.0","id":2,"method":"tools/list"}\n
```

### Rules

1. Each message is a complete JSON object on a single line
2. Messages are delimited by newline (`\n`)
3. Messages MUST NOT contain embedded newlines
4. Encoding MUST be UTF-8

## Message Types

### Request

Has `id` and `method`, expects response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list"
}
```

### Notification

Has `method` but no `id`, no response expected:

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

### Response (Success)

Has `id` and `result`:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {...}
}
```

### Response (Error)

Has `id` and `error`:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Invalid Request"
  }
}
```

## JSON-RPC Error Codes

| Code | Meaning |
|------|---------|
| -32700 | Parse error - Invalid JSON |
| -32600 | Invalid Request - Not a valid JSON-RPC request |
| -32601 | Method not found |
| -32602 | Invalid params |
| -32603 | Internal error |

## Server Constraints

1. Server MUST NOT write non-JSON-RPC content to `stdout`
2. Server MUST read complete lines from `stdin`
3. Server MUST respond to requests in a timely manner
4. Server MAY write log messages to `stderr`

## Lifecycle

```
Client                          Server
   |                               |
   |--- spawn server process ----->|
   |                               |
   |<-- server ready (no output) --|
   |                               |
   |-- initialize request -------->|
   |<-- initialize response -------|
   |                               |
   |-- initialized notification -->|
   |                               |
   |-- tools/list --------------->|
   |<-- tools list ----------------|
   |                               |
   |-- tools/call ---------------->|
   |<-- tool result ---------------|
   |                               |
   |-- (close stdin) ------------->|
   |                               |
   |<-- server exits --------------|
```

## Graceful Shutdown

When `stdin` is closed (EOF):
1. Server completes any in-progress operations
2. Server exits with code 0
