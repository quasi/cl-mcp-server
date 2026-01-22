# CL-MCP-Server Canon

Model Context Protocol (MCP) server implemented in Common Lisp. Enables Claude to send Common Lisp code for evaluation in a running Lisp instance.

## What is This?

This is a **Canon** - a complete, authoritative specification of the cl-mcp-server system. The Canon defines what the system does (behavior), not how it does it (implementation).

Implementations can be generated from this Canon and verified against it.

## Structure

```
canon/
├── canon.yaml          # Manifest: metadata, features list
├── core/               # Shared definitions
│   ├── foundation/     # Vocabulary, ontology, invariants
│   ├── contracts/      # Shared data types (JSON-RPC, MCP types)
│   └── context/        # Cross-cutting decisions
├── features/           # Feature-specific specifications
│   ├── mcp-protocol/   # JSON-RPC 2.0 over stdio
│   ├── code-evaluator/ # Lisp expression evaluation
│   ├── session-management/ # State persistence
│   └── error-handling/ # Condition capture and reporting
└── verification/       # Integration and system tests
```

## Core Concepts

- **MCP Server**: Main process handling protocol communication
- **Session**: Stateful evaluation context persisting across requests
- **Evaluation**: Single code execution within session context
- **Condition**: Lisp's error/warning mechanism, captured and reported

## Getting Started

1. **Understand the Domain**: Read `core/foundation/vocabulary.md`
2. **Review Invariants**: Check `core/foundation/invariants.md` for system rules
3. **Explore Features**: Each feature has its own vocabulary and contracts
4. **Add Specifications**: Use `canon-genesis` for guided development

## Features

| Feature | Status | Description |
|---------|--------|-------------|
| mcp-protocol | draft | JSON-RPC 2.0 communication over stdio |
| code-evaluator | draft | Common Lisp expression evaluation |
| session-management | draft | Persistent REPL environment |
| error-handling | draft | Condition capture and reporting |

## Using the Canon

- **Continue Specification**: Use `canon-genesis` or `canon-specify` skill
- **Generate Implementation**: Use `canon-implement` skill with target language
- **Verify Implementation**: Use `canon-verify` skill against implementation
- **Evolve Specification**: Use `canon-evolve` skill for changes

---

*This Canon was bootstrapped on 2026-01-22.*
