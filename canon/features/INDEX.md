# Features Index

**All features in the CL-MCP-Server Canon.**

## Overview

| Feature | Status | Priority | Description |
|---------|--------|----------|-------------|
| [mcp-protocol](#mcp-protocol) | draft | P0 | JSON-RPC 2.0 communication over stdio |
| [code-evaluator](#code-evaluator) | draft | P0 | Common Lisp expression evaluation with safety controls |
| [session-management](#session-management) | draft | P0 | Persistent REPL environment with state tracking |
| [error-handling](#error-handling) | draft | P0 | Condition capture and structured error reporting |

## Feature Details

### mcp-protocol

**Purpose**: Implement the Model Context Protocol for communication with Claude.

**Location**: `mcp-protocol/`

**Status**: draft

**Quick Navigation**:

| Need to... | Read |
|------------|------|
| Implement initialization handshake | `contracts/initialization.md` |
| List available tools | `contracts/tools-list.md` |
| Handle tool call requests | `contracts/tools-call.md` |
| Understand stdio transport | `contracts/transport.md` |
| Test initialization flow | `scenarios/initialization-handshake.md` |
| Handle malformed requests | `scenarios/malformed-request.md` |

**Contracts**: 4
- `initialization.md` - MCP handshake
- `tools-list.md` - Tool enumeration
- `tools-call.md` - Tool invocation
- `transport.md` - stdio communication

**Scenarios**: 2
- `initialization-handshake.md` - Startup protocol
- `malformed-request.md` - Error handling

**Dependencies**: None (foundation layer)

---

### code-evaluator

**Purpose**: Execute Common Lisp code safely in a persistent session.

**Location**: `code-evaluator/`

**Status**: draft

**Quick Navigation**:

| Need to... | Read |
|------------|------|
| Implement evaluate-lisp tool | `contracts/evaluate-lisp-tool.md` |
| Test basic expressions | `scenarios/basic-evaluation.md` |
| Handle function/variable definitions | `scenarios/definitions.md` |
| Capture stdout/stderr output | `scenarios/output-capture.md` |

**Contracts**: 1
- `evaluate-lisp-tool.md` - Primary evaluation tool

**Scenarios**: 3
- `basic-evaluation.md` - Simple expressions
- `definitions.md` - Persistent definitions
- `output-capture.md` - Stream handling

**Dependencies**:
- `mcp-protocol` (for tool interface)
- `session-management` (for state)
- `error-handling` (for conditions)

---

### session-management

**Purpose**: Maintain persistent REPL state across evaluations.

**Location**: `session-management/`

**Status**: draft

**Quick Navigation**:

| Need to... | Read |
|------------|------|
| Understand session state model | `contracts/session-state.md` |
| Test state persistence | `scenarios/state-persistence.md` |
| Implement session reset | `scenarios/session-reset.md` |

**Contracts**: 1
- `session-state.md` - State management

**Scenarios**: 2
- `state-persistence.md` - Definitions persist
- `session-reset.md` - Clean slate

**Dependencies**:
- `mcp-protocol` (communication layer)

---

### error-handling

**Purpose**: Capture Lisp conditions and report them to Claude.

**Location**: `error-handling/`

**Status**: draft

**Quick Navigation**:

| Need to... | Read |
|------------|------|
| Format condition reports | `contracts/condition-report.md` |
| Test error scenarios | `scenarios/evaluation-errors.md` |

**Contracts**: 1
- `condition-report.md` - Error formatting

**Scenarios**: 1
- `evaluation-errors.md` - Error cases

**Dependencies**:
- `mcp-protocol` (response format)

---

## Dependency Graph

```dot
digraph FeatureDependencies {
    rankdir=TB;
    node [shape=box];

    mcp [label="mcp-protocol\n(foundation)"];
    evaluator [label="code-evaluator"];
    session [label="session-management"];
    errors [label="error-handling"];

    evaluator -> mcp;
    evaluator -> session;
    evaluator -> errors;
    session -> mcp;
    errors -> mcp;
}
```

**Implementation Order** (based on dependencies):
1. `mcp-protocol` (no dependencies)
2. `session-management` (depends on mcp-protocol)
3. `error-handling` (depends on mcp-protocol)
4. `code-evaluator` (depends on all three)

## Feature Statistics

| Metric | Count |
|--------|-------|
| Total features | 4 |
| Total contracts | 7 |
| Total scenarios | 8 |
| Total properties | 0 (defined in contracts) |
| Total decisions | 0 (no decision logs yet) |

## Cross-Feature Concepts

### Common Types

Shared type definitions in `core/contracts/shared-types.md`:
- JSON-RPC message format
- Tool definition structure
- Response envelope

### Shared Vocabulary

Terms used across features (see `core/foundation/vocabulary.md`):
- **Session**: Persistent evaluation context
- **Evaluation**: Single code execution
- **Condition**: Error/warning representation
- **Transport**: Communication channel

---

**Last Updated**: 2026-01-22
