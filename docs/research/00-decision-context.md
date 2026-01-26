# Decision Context: CL-MCP-Server Direction

## Research Question

What features, design patterns, and architectural decisions should inform the future direction of cl-mcp-server?

## Context

We are building a Common Lisp MCP server to empower Claude (and similar AI agents) with the ability to interact with a running Lisp image (SBCL). This research compares existing CL MCP implementations to:

1. Identify proven patterns and avoid reinventing wheels
2. Discover features we may want to adopt
3. Understand design trade-offs others have made
4. Find gaps in the ecosystem we could fill

## Constraints

| Constraint | Description |
|------------|-------------|
| Implementation | SBCL primary target (portable CL secondary) |
| Transport | stdio primary (MCP standard), HTTP/SSE optional |
| Safety | Must provide guardrails for code evaluation |
| Simplicity | Prefer minimal dependencies, idiomatic CL |
| Agent-Focus | Designed for AI agent interaction, not human REPL |

## Priorities

| Priority | Dimension | Rationale |
|----------|-----------|-----------|
| 1 | Protocol Correctness | Must be MCP-compliant for Claude integration |
| 2 | Safety & Sandboxing | Running arbitrary code requires protection |
| 3 | Developer Experience | Easy to extend, debug, and maintain |
| 4 | Feature Completeness | Tools, resources, prompts as needed |
| 5 | Performance | Important but secondary to correctness |

## Comparison Dimensions

### Core Protocol
- MCP version support
- JSON-RPC 2.0 compliance
- Transport mechanisms
- Initialization handshake

### Capabilities
- Tools implementation
- Resources implementation
- Prompts implementation
- Sampling support

### Lisp Integration
- Code evaluation mechanism
- Package handling
- Definition persistence
- Output capture
- Error handling

### Safety
- Sandboxing approach
- Resource limits
- Forbidden operations
- Timeout handling

### Architecture
- Threading model
- Session management
- Extensibility hooks
- Dependency count

### Ecosystem
- Documentation quality
- Test coverage
- Maintenance activity
- Community adoption

## Sources Under Study

1. **belyak/mcp-srv-lisp** - GitHub
2. **gornskew/lisply-mcp** - GitHub
3. **40ants/mcp** - GitHub
4. **cl-ai-project/cl-mcp** - GitHub
5. **xbill999 Medium article** - MCP with Lisp + Cloud Run
6. **Our implementation** - cl-mcp-server (baseline)

## Decision Gravity

**Medium-High**: Architectural decisions made now will influence the library's evolution. However, being early stage means we can still pivot.

## Reversibility

**Medium**: Core protocol decisions are harder to change; feature additions are easier.
