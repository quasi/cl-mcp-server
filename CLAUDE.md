# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Model Context Protocol (MCP) server implemented in Common Lisp. The server allows Claude to send Common Lisp code for evaluation in a running Lisp instance, enabling interactive Lisp development and execution.

## Architecture

### Core Components

- **MCP Server**: Implements the Model Context Protocol, handling JSON-RPC 2.0 communication over stdio
- **Code Evaluator**: Evaluates Common Lisp expressions in a sandboxed or controlled environment
- **Session Management**: Maintains state across multiple evaluation requests
- **Error Handling**: Captures and reports Lisp conditions/errors in a format Claude can understand

### Communication Flow

1. Claude sends JSON-RPC requests via stdin
2. Server parses requests and routes to appropriate handlers
3. For code evaluation requests, the Lisp expression is evaluated
4. Results (or errors) are serialized and sent back via stdout as JSON-RPC responses
5. State is maintained between evaluations within a session

## Development Commands

### Build System

```bash
# Load the system in SBCL
sbcl --load cl-mcp-server.asd --eval "(ql:quickload :cl-mcp-server)"

# Run the server
sbcl --load cl-mcp-server.asd --eval "(ql:quickload :cl-mcp-server)" --eval "(cl-mcp-server:start)"
```

### Testing

```bash
# Run all tests (once test framework is added)
sbcl --load cl-mcp-server.asd --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(asdf:test-system :cl-mcp-server)"

# Run specific test suite
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests:evaluator-suite)"
```

## Implementation Guidelines

### MCP Protocol Conformance

- All communication must be valid JSON-RPC 2.0
- Use stdio for transport (stdin for requests, stdout for responses)
- Implement required MCP methods: `initialize`, `initialized`, `tools/list`, `tools/call`
- The primary tool should be named `evaluate-lisp` or similar

### Code Evaluation Safety

- Consider using `sb-ext:restrict-compiler-policy` for SBCL to limit dangerous operations
- Catch all conditions during evaluation to prevent server crashes
- Return both successful results and error information in structured format
- Preserve the distinction between return values, stdout output, and stderr output

### State Management

- Maintain a persistent REPL-like environment where definitions persist between evaluations
- Each evaluation should occur in a consistent package context
- Consider supporting package switching via special forms or commands
- Track loaded systems and defined symbols for context

### Error Reporting

- Capture condition type, message, and backtrace
- Format backtraces in a readable way for Claude
- Distinguish between compilation errors, runtime errors, and warnings
- Return partial results when possible (e.g., warnings alongside successful evaluation)

## Common Lisp Conventions

- Use `defpackage` with explicit exports for the public API
- Follow standard Common Lisp naming: `*special-variables*`, `+constants+`, `make-foo` constructors
- Add ABOUTME comments to each file explaining its purpose
- Use ASDF as the build system with proper system definitions in `.asd` files

## Testing Strategy

- Use FiveAM or similar testing framework
- Test JSON-RPC parsing and response generation
- Test code evaluation with various expressions (symbols, lists, errors, multiline)
- Test session state persistence
- Mock MCP client interactions for integration tests

## Issue Tracking

This project uses **bd (beads)** for issue tracking.
Run `bd prime` for workflow context, or install hooks (`bd hooks install`) for auto-injection.

**Quick reference:**
- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd close <id>` - Complete work
- `bd sync` - Sync with git (run at session end)

For full workflow details: `bd prime`

