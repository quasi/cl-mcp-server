# Core Index

**Foundation layer for the CL-MCP-Server Canon.**

## Purpose

The `core/` directory contains the foundational artifacts that all features depend on:

- **Vocabulary**: Domain terms and their definitions
- **Ontology**: Entity relationships and cardinalities
- **Invariants**: System-wide rules that must always hold
- **Shared Types**: Common type definitions used across features

## Quick Navigation

| Need to... | Read |
|------------|------|
| Understand domain terminology | `foundation/vocabulary.md` |
| See how entities relate | `foundation/ontology.md` |
| Learn system-wide rules | `foundation/invariants.md` |
| Find common type definitions | `contracts/shared-types.md` |

## Contents

| File | Purpose | Lines | Key Concepts |
|------|---------|-------|--------------|
| `foundation/vocabulary.md` | Domain terms | ~114 | MCP Server, Session, Evaluation, Transport, Condition, Tool, Package |
| `foundation/ontology.md` | Entity relationships | ~84 | Cardinalities, communication flow diagrams |
| `foundation/invariants.md` | System-wide properties | ~74 | 6 critical invariants (INV-001 to INV-006) |
| `contracts/shared-types.md` | Common types | TBD | JSON-RPC structures, tool schemas |

## Key Concepts

### Vocabulary

**7 core domain terms** defined in `foundation/vocabulary.md`:

1. **MCP Server** - Main process implementing the protocol
2. **Session** - Stateful evaluation context (1:1 with server)
3. **Evaluation** - Single code execution request (N:1 with session)
4. **Transport** - stdio communication channel
5. **Condition** - Lisp error/warning representation
6. **Tool** - MCP capability exposed to Claude
7. **Package** - Lisp namespace for symbols

Each term includes:
- Definition
- Relationships (cardinality)
- Properties
- Invariants
- Semantic boundaries (what it is NOT)

### Ontology

**Entity relationship model** in `foundation/ontology.md`:

```
MCP Server
  ├─ maintains → Session (1:1)
  ├─ uses → Transport (1:1)
  └─ exposes → Tools (1:N)

Session
  ├─ has current → Package (1:1)
  └─ contains → Evaluations (1:N)

Evaluation
  ├─ produces → Result (1:1)
  ├─ captures → Output (1:1)
  └─ may signal → Conditions (1:0..N)
```

Includes:
- Graphviz diagrams (entity relationships, communication flow)
- Cardinality table

### Invariants

**6 system-wide rules** in `foundation/invariants.md`:

| ID | Rule | Rationale |
|----|------|-----------|
| INV-001 | Request-Response Guarantee | Every valid JSON-RPC request receives exactly one response |
| INV-002 | Server Stability | Server never terminates due to evaluation errors |
| INV-003 | Session State Persistence | Definitions persist across evaluations |
| INV-004 | Output Stream Separation | stdout/stderr/values are distinguishable |
| INV-005 | Protocol Conformance | All messages are valid JSON-RPC 2.0 |
| INV-006 | Condition Type Preservation | Error condition types are preserved in reports |

Each invariant includes:
- Statement (the rule)
- Rationale (why it matters)
- Violation Impact (what breaks if violated)

### Shared Types

Common type definitions used across features (in `contracts/shared-types.md`):

- JSON-RPC 2.0 message envelope
- Tool definition schema
- Response content structure
- Error object format

## Dependencies

**This directory has no dependencies** - it is the foundation layer.

**All features depend on core**:

```
features/mcp-protocol     ──depends on──> core/
features/code-evaluator   ──depends on──> core/
features/session-mgmt     ──depends on──> core/
features/error-handling   ──depends on──> core/
```

## Usage Patterns

### For Implementation

When implementing any feature:

1. **Start with vocabulary** - understand the terms
2. **Study ontology** - see how entities relate
3. **Memorize invariants** - these are non-negotiable
4. **Reference shared types** - reuse common definitions

### For Verification

When verifying implementation:

1. Check that code matches vocabulary definitions
2. Verify entity relationships match ontology
3. Assert all 6 invariants hold
4. Confirm types match shared definitions

## Cross-References

### Terms Used in Features

| Term | Used in Features |
|------|------------------|
| MCP Server | mcp-protocol, all features |
| Session | session-management, code-evaluator |
| Evaluation | code-evaluator, error-handling |
| Transport | mcp-protocol |
| Condition | error-handling, code-evaluator |
| Tool | mcp-protocol, code-evaluator |
| Package | session-management, code-evaluator |

### Invariants by Feature

| Invariant | Primary Feature | Secondary Features |
|-----------|----------------|-------------------|
| INV-001 | mcp-protocol | all features |
| INV-002 | error-handling | code-evaluator |
| INV-003 | session-management | code-evaluator |
| INV-004 | code-evaluator | error-handling |
| INV-005 | mcp-protocol | all features |
| INV-006 | error-handling | code-evaluator |

## Evolution

The core foundation is **relatively stable** but not immutable.

**Add new terms** when:
- Implementation reveals a missing abstraction
- Features need a shared concept

**Add new invariants** when:
- A critical property is discovered
- A bug reveals an unstated assumption

**Update ontology** when:
- Entity relationships change
- New cardinality constraints are found

Use `canon-evolve` to update core artifacts systematically.

---

**Last Updated**: 2026-01-22
