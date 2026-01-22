# CL-MCP-Server Implementation Plans

Implementation plans for the Common Lisp MCP Server, organized by dependency order.

## Plan Hierarchy

```
                    ┌─────────────────────────────────────┐
                    │   Plan 00: Project Bootstrap        │
                    │   ASDF, packages, dependencies      │
                    └─────────────────────────────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────┐
                    │   Plan 01: JSON-RPC Core            │
                    │   Parsing, encoding, transport      │
                    └─────────────────────────────────────┘
                                      │
              ┌───────────────────────┼───────────────────────┐
              ▼                       ▼                       │
    ┌───────────────────┐   ┌───────────────────┐            │
    │ Plan 02: Error    │   │ Plan 03: Session  │            │
    │ Handling          │   │ Management        │            │
    └───────────────────┘   └───────────────────┘            │
              │                       │                       │
              └───────────┬───────────┘                       │
                          ▼                                   │
                ┌─────────────────────────────────────┐       │
                │   Plan 04: Code Evaluator           │       │
                │   Evaluation, output capture        │       │
                └─────────────────────────────────────┘       │
                                      │                       │
                                      ▼                       │
                ┌─────────────────────────────────────┐       │
                │   Plan 05: MCP Server Integration   │◄──────┘
                │   Tools, handlers, main loop        │
                └─────────────────────────────────────┘
```

## Plan Index

| # | Plan | Description | Dependencies | Status |
|---|------|-------------|--------------|--------|
| 00 | [project-bootstrap](2026-01-22-00-project-bootstrap.md) | ASDF system, packages, directory structure | None | TODO |
| 01 | [json-rpc-core](2026-01-22-01-json-rpc-core.md) | JSON-RPC 2.0 messages, parsing, transport | 00 | TODO |
| 02 | [error-handling](2026-01-22-02-error-handling.md) | Condition capture, backtrace formatting | 01 | TODO |
| 03 | [session-management](2026-01-22-03-session-management.md) | Session state, definitions, reset | 01 | TODO |
| 04 | [code-evaluator](2026-01-22-04-code-evaluator.md) | Code evaluation, output capture | 02, 03 | TODO |
| 05 | [mcp-server-integration](2026-01-22-05-mcp-server-integration.md) | Tools, handlers, server loop | 04 | TODO |

## Execution Order

Plans must be executed in this order due to dependencies:

1. **00-project-bootstrap** - Creates project structure (BLOCKING)
2. **01-json-rpc-core** - Foundation for all communication (BLOCKING)
3. **02-error-handling** AND **03-session-management** - Can run in parallel
4. **04-code-evaluator** - Requires 02 and 03
5. **05-mcp-server-integration** - Final integration

## How to Execute Plans

Each plan includes bite-sized tasks with TDD workflow:

1. Write failing test
2. Run test to verify failure
3. Implement minimal code
4. Run test to verify pass
5. Commit

Use the `superpowers:executing-plans` skill for automated execution with review checkpoints.

## Tracking

- **bd issues**: Each plan has a corresponding bd issue
- **TodoWrite**: Used during execution for task tracking
- **Git commits**: Frequent commits per task

## Spec Reference

All plans reference specifications in `canon/`:
- `canon/core/contracts/shared-types.md` - JSON-RPC types
- `canon/features/mcp-protocol/` - Protocol contracts and scenarios
- `canon/features/code-evaluator/` - Evaluation contracts and scenarios
- `canon/features/session-management/` - Session contracts and scenarios
- `canon/features/error-handling/` - Error contracts and scenarios
