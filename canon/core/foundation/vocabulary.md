# Core Vocabulary

Fundamental terms for the CL-MCP-Server system.

---

## MCP Server

The **MCP Server** is the main process that implements the Model Context Protocol,
handling communication between Claude and the Common Lisp evaluation environment.

### Relationships

| Relationship | Cardinality | Notes |
|-------------|-------------|-------|
| MCP Server → Session | 1:1 | One session per server instance |
| MCP Server → Transport | 1:1 | Uses stdio transport |

### Invariants

1. Server must respond to every valid JSON-RPC request
2. Server must never crash on malformed input

---

## Session

A **Session** represents the stateful evaluation context maintained across
multiple code evaluation requests. Definitions, variables, and loaded systems
persist within a session.

### Relationships

| Relationship | Cardinality | Notes |
|-------------|-------------|-------|
| Session → Package | 1:1 | Current package context |
| Session → Evaluation | 1:N | Evaluations within session |

### Invariants

1. Session state persists until server termination
2. Each evaluation inherits the accumulated session state

---

## Evaluation

An **Evaluation** is a single request to execute Common Lisp code within the
current session context.

### Relationships

| Relationship | Cardinality | Notes |
|-------------|-------------|-------|
| Evaluation → Result | 1:1 | Every evaluation produces a result |
| Evaluation → Session | N:1 | Belongs to session |

### Properties

- **Code**: The Lisp expression(s) to evaluate
- **Result**: The return value(s) or error condition
- **Output**: Captured stdout/stderr during evaluation

---

## Transport

The **Transport** is the communication channel between Claude and the MCP server.
This implementation uses stdio (stdin for requests, stdout for responses).

### Semantic Boundaries

**Transport is NOT**:
- **HTTP/WebSocket**: This server uses stdio only
- **Bidirectional streaming**: Request-response pattern only

---

## Condition

A **Condition** is Common Lisp's mechanism for representing errors, warnings,
and other exceptional situations during evaluation.

### Properties

- **Type**: The condition class (e.g., `error`, `warning`, `style-warning`)
- **Message**: Human-readable description
- **Backtrace**: Stack trace at point of signaling

---

## Tool

A **Tool** in MCP terminology is a capability the server exposes to Claude.
The primary tool is `evaluate-lisp` for code execution.

### Relationships

| Relationship | Cardinality | Notes |
|-------------|-------------|-------|
| MCP Server → Tool | 1:N | Server exposes multiple tools |

---

## Package

A **Package** in Common Lisp is a namespace for symbols. The session maintains
a current package context for evaluations.

### Invariants

1. Default package is `CL-USER` or a server-defined package
2. Package can be switched via in-band commands or special forms
