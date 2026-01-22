# MCP Protocol Vocabulary

Terms specific to the protocol layer.

---

## Method

A **Method** is a named operation the server can perform, identified by a
string in the JSON-RPC request. MCP defines standard methods like `initialize`,
`tools/list`, and `tools/call`.

---

## Notification

A **Notification** is a JSON-RPC message without an `id` field. Notifications
do not expect a response. The `initialized` message is an example.

---

## Capability

A **Capability** declares what features the server supports. Capabilities are
exchanged during initialization handshake.
