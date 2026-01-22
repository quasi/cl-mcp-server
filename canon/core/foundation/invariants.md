# Core Invariants

System-wide rules that must always hold.

---

## INV-001: Request-Response Guarantee

**Statement**: Every valid JSON-RPC 2.0 request MUST receive exactly one response.

**Rationale**: MCP protocol requires reliable request-response semantics.
Claude cannot function if requests are silently dropped.

**Violation Impact**: Claude loses synchronization with server state.

---

## INV-002: Server Stability

**Statement**: The server process MUST NOT terminate due to evaluation errors.

**Rationale**: User code may contain bugs. The server must catch all conditions
and report them gracefully rather than crashing.

**Violation Impact**: Session state lost, Claude loses REPL context.

---

## INV-003: Session State Persistence

**Statement**: Definitions and bindings made in one evaluation MUST be available
in subsequent evaluations within the same session.

**Rationale**: This enables incremental development - define a function, then
use it later. This is the fundamental REPL experience.

**Violation Impact**: Users cannot build up state interactively.

---

## INV-004: Output Stream Separation

**Statement**: Return values, stdout output, and stderr output MUST be
distinguishable in the response.

**Rationale**: Claude needs to understand what is a computed value vs.
side-effect output vs. error/warning messages.

**Violation Impact**: Ambiguous results confuse Claude's interpretation.

---

## INV-005: Protocol Conformance

**Statement**: All messages MUST conform to JSON-RPC 2.0 specification.

**Rationale**: MCP is built on JSON-RPC 2.0. Non-conforming messages will
be rejected or misinterpreted.

**Violation Impact**: Communication failure with Claude.

---

## INV-006: Condition Type Preservation

**Statement**: When an error occurs, the condition type (error class) MUST be
preserved and reported, not just the message text.

**Rationale**: Different condition types require different handling. A
`type-error` vs `file-error` vs `undefined-function` conveys important
diagnostic information.

**Violation Impact**: Claude cannot provide accurate error diagnosis.
