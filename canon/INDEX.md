# Canon Index

**Project**: cl-mcp-server
**Canon Version**: 0.1.0
**Protocol**: MCP 2025-03-26

## Quick Navigation

| I want to... | Start here |
|--------------|------------|
| Understand what this project does | `core/foundation/vocabulary.md` |
| See the domain model relationships | `core/foundation/ontology.md` |
| Learn system-wide invariants | `core/foundation/invariants.md` |
| Implement MCP protocol | `features/mcp-protocol/` |
| Implement code evaluation | `features/code-evaluator/` |
| Handle errors and conditions | `features/error-handling/` |
| Manage session state | `features/session-management/` |
| Find all contracts | Search `features/*/contracts/*.md` |

## Structure

```
canon/
├── core/                          # Shared foundation
│   ├── foundation/
│   │   ├── vocabulary.md          # Domain terms (~114 lines)
│   │   ├── ontology.md            # Entity relationships (~84 lines)
│   │   └── invariants.md          # System-wide properties (~74 lines)
│   └── contracts/
│       └── shared-types.md        # Common type definitions
│
├── features/                      # Feature specifications
│   ├── mcp-protocol/              # JSON-RPC over stdio
│   ├── code-evaluator/            # Lisp code execution
│   ├── session-management/        # State persistence
│   └── error-handling/            # Condition capture
│
└── verification/                  # Verification artifacts
```

## Features

| Feature | Status | Description | Contracts | Scenarios |
|---------|--------|-------------|-----------|-----------|
| **mcp-protocol** | draft | JSON-RPC 2.0 communication over stdio | 4 | 2 |
| **code-evaluator** | draft | Common Lisp expression evaluation with safety | 1 | 3 |
| **session-management** | draft | Persistent REPL environment with state tracking | 1 | 2 |
| **error-handling** | draft | Condition capture and structured reporting | 1 | 1 |

See `features/INDEX.md` for feature details.

## Core Concepts

### Key Terms

From `core/foundation/vocabulary.md`:

- **MCP Server**: Main process implementing Model Context Protocol
- **Session**: Stateful evaluation context (definitions persist)
- **Evaluation**: Single code execution request
- **Transport**: stdio communication channel
- **Condition**: Lisp error/warning mechanism
- **Tool**: MCP capability (primary: `evaluate-lisp`)
- **Package**: Lisp namespace for symbols

### Entity Relationships

```
MCP Server (1) ──maintains──> (1) Session
           (1) ──uses──────> (1) Transport
           (1) ──exposes───> (N) Tools

Session    (1) ──current───> (1) Package
           (1) ──contains──> (N) Evaluations

Evaluation (1) ──produces──> (1) Result
           (1) ──captures──> (1) Output
           (1) ──may signal> (0..N) Conditions
```

See `core/foundation/ontology.md` for full diagrams.

## System-Wide Invariants

Six critical invariants must always hold:

1. **INV-001**: Every valid JSON-RPC request receives exactly one response
2. **INV-002**: Server never terminates due to evaluation errors
3. **INV-003**: Session state persists across evaluations
4. **INV-004**: Output streams (stdout/stderr/values) are distinguishable
5. **INV-005**: All messages conform to JSON-RPC 2.0
6. **INV-006**: Condition types are preserved in error reports

See `core/foundation/invariants.md` for detailed rationale.

## Dependencies

```
┌─────────────────────────────────────┐
│            ALL FEATURES             │
│                  │                  │
│                  ▼                  │
│         core/foundation             │
│    (vocabulary, ontology,           │
│     invariants, shared types)       │
└─────────────────────────────────────┘

Feature interdependencies:
- code-evaluator depends on: mcp-protocol, session-management, error-handling
- error-handling depends on: mcp-protocol
- session-management depends on: mcp-protocol
```

## Navigation by Task

| Task | Files to Read |
|------|---------------|
| **Understand the domain** | `core/foundation/vocabulary.md` → `ontology.md` |
| **Implement initialization** | `features/mcp-protocol/contracts/initialization.md` → `scenarios/initialization-handshake.md` |
| **Implement tool listing** | `features/mcp-protocol/contracts/tools-list.md` |
| **Implement code evaluation** | `features/code-evaluator/contracts/evaluate-lisp-tool.md` → `scenarios/basic-evaluation.md` |
| **Handle evaluation errors** | `features/error-handling/contracts/condition-report.md` → `scenarios/evaluation-errors.md` |
| **Capture stdout/stderr** | `features/code-evaluator/scenarios/output-capture.md` |
| **Preserve session state** | `features/session-management/contracts/session-state.md` → `scenarios/state-persistence.md` |
| **Debug protocol issues** | `features/mcp-protocol/scenarios/malformed-request.md` |

## Verification

Verification artifacts are in `verification/` directory:
- Test plans
- Validation criteria
- Conformance checks

## Meta

**Created**: 2026-01-22
**Last Updated**: 2026-01-22
**Authors**: Baba (quasi)

This Canon is a living specification. As the implementation evolves, Canon artifacts are updated via `canon-evolve` to reflect discovered constraints, edge cases, and design decisions.
