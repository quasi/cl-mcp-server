---
type: property
name: protocol-conformance
version: 1.0.0
feature: mcp-protocol
covers:
  - contracts/initialization
  - contracts/tools-list
  - contracts/tools-call
  - contracts/transport
---

# Protocol Conformance Property

## Statement

**For all** messages sent or received by the server,
**if** the message is part of MCP communication,
**then** the message MUST conform to JSON-RPC 2.0 specification.

## Formal Expression

```
∀ message ∈ Messages :
  (message.sent_by_server ∨ message.received_by_server) ⟹
    is_valid_jsonrpc_2_0(message)

where is_valid_jsonrpc_2_0(m) ≡
  m.jsonrpc = "2.0" ∧
  (has_field(m, "method") ∨ has_field(m, "result") ∨ has_field(m, "error")) ∧
  (¬has_field(m, "error") ∨ ¬has_field(m, "result"))
```

## Informal Explanation

Every message sent or received by the MCP server must be valid JSON-RPC 2.0. This means:

1. It must have `jsonrpc: "2.0"` field
2. Requests must have `method` and optionally `id` and `params`
3. Responses must have `id` and either `result` or `error` (but not both)
4. Notifications must have `method` but no `id`

The server must produce conforming messages and must validate incoming messages.

## Rationale

MCP is built on top of JSON-RPC 2.0. Non-conforming messages will be rejected by Claude Code or cause communication failures. Strict conformance ensures interoperability with any MCP client.

## Counterexample Shape

If this property is violated, you might see:
- Messages missing the `jsonrpc` field
- Responses with both `result` and `error` fields
- Requests with incorrect field types
- Messages that cannot be parsed by standard JSON-RPC libraries

## Verification Approach

**Generator**: Generate random valid and invalid JSON-RPC messages

**Assertion**:
- Valid messages are accepted and processed
- Invalid messages produce JSON-RPC error responses
- All server responses pass JSON-RPC 2.0 validation

**Shrinking**: Standard shrinking to find minimal non-conforming message

**Implementation Notes**:
```lisp
;; Test with json-rpc-validator library
(defun verify-protocol-conformance (message)
  (and (assoc :jsonrpc message)
       (string= (cdr (assoc :jsonrpc message)) "2.0")
       (or (assoc :method message)    ; request/notification
           (assoc :result message)    ; success response
           (assoc :error message))    ; error response
       (not (and (assoc :result message)
                 (assoc :error message)))))
```
