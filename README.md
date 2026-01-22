# CL-MCP-Server

**Model Context Protocol server for Common Lisp code evaluation**

Enable Claude and other AI agents to evaluate Common Lisp code in a persistent, stateful REPL session over the Model Context Protocol (MCP).

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

---

## What is This?

CL-MCP-Server is an MCP server implementation that provides Claude with the ability to:

- **Evaluate Common Lisp expressions** in a live REPL environment
- **Maintain persistent state** across evaluations (functions, variables, loaded systems)
- **Capture rich output** (return values, stdout, stderr, warnings, backtraces)
- **Handle errors gracefully** using Common Lisp's condition system
- **Support incremental development** with stateful session management

Unlike one-shot code execution, CL-MCP-Server provides a full REPL experience where definitions accumulate and state persists, enabling interactive exploratory programming through Claude.

## Value Proposition

### For Claude Users

- **Persistent REPL**: Define functions once, use them repeatedly in the same session
- **Rich Error Reporting**: Get detailed backtraces and condition information when things go wrong
- **Stream Separation**: Clearly distinguish between return values, printed output, and warnings
- **Safe Execution**: Server never crashes—all user code errors are caught and reported

### For Developers

- **Standard Protocol**: Uses MCP and JSON-RPC 2.0 for interoperability
- **Formally Specified**: Complete Canon specification in `canon/` directory
- **Well-Tested**: Comprehensive test suite covering protocol, evaluation, and error handling
- **Extensible**: Clean architecture supports adding new tools and capabilities

### For AI Agents

- **Formal Contracts**: Machine-readable specifications in `canon/features/`
- **Predictable Behavior**: Documented invariants and properties
- **JSON Schema**: Structured request/response formats
- **Standard Transport**: stdio-based communication

## Quick Start

### Prerequisites

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/) for dependency management
- [Claude Code](https://claude.ai/code) or compatible MCP client

### Installation

1. Clone the repository:

```bash
git clone https://github.com/quasi/cl-mcp-server.git
cd cl-mcp-server
```

2. Load dependencies (Quicklisp will install them automatically):

```bash
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)" \
     --quit
```

3. Configure Claude Code:

```bash
claude mcp add --scope user --transport stdio lisp -- sbcl --script /path/to/cl-mcp-server/run-server.lisp

```

4. Configure Claude Desktop to use the server (optionally):

```json
{
  "mcpServers": {
    "lisp": {
      "command": "sbcl",
      "args": [
        "--load", "/path/to/cl-mcp-server/run-server.lisp"
      ]
    }
  }
}
```

### Usage Example

```
User: Please evaluate (+ 1 2 3)
Claude: => 6

User: Define a function to calculate factorial
Claude: (evaluating)
        (defun factorial (n)
          (if (<= n 1) 1 (* n (factorial (- n 1)))))
        => FACTORIAL

User: What is 10 factorial?
Claude: (evaluating) (factorial 10)
        => 3628800
```

See the [Quickstart Guide](docs/quickstart.md) for a complete walkthrough.

## Documentation

### For Users

- **[Quickstart](docs/quickstart.md)** - Get running in 5 minutes
- **[Tutorial: First REPL Session](docs/tutorials/01-first-session.md)** - Learn by building a temperature converter
- **[Full Documentation](docs/README.md)** - Complete user guide

### For Contributors

- **[AGENT.md](AGENT.md)** - Contributor guidelines, build commands, code conventions
- **[canon/INDEX.md](canon/INDEX.md)** - Navigate formal specifications
- **[Architecture](docs/explanation/architecture.md)** - System design and rationale

### For External Agents

- **[canon/features/](canon/features/)** - Formal API specifications
- **[canon/core/foundation/vocabulary.md](canon/core/foundation/vocabulary.md)** - Domain model
- **[MCP Protocol Contracts](canon/features/mcp-protocol/contracts/)** - Protocol details

## Features

- ✓ **MCP Protocol**: Standards-compliant JSON-RPC 2.0 over stdio
- ✓ **Persistent Session**: State persists across all evaluations
- ✓ **Rich Output**: Separates return values, stdout, stderr, and warnings
- ✓ **Error Handling**: Captures and reports conditions with backtraces
- ✓ **Multiple Values**: Full support for Common Lisp's multiple return values
- ✓ **Safety**: Server isolation prevents user code from crashing the server
- ✓ **Stream Capture**: All output streams are captured during evaluation

## Architecture

```
┌──────────────────────────────────────┐
│         MCP Client (Claude)          │
└──────────────┬───────────────────────┘
               │ JSON-RPC over stdio
               │
┌──────────────▼───────────────────────┐
│         CL-MCP-Server                 │
│  ┌────────────────────────────────┐   │
│  │  Protocol Layer (JSON-RPC)     │   │
│  └──────────────┬─────────────────┘   │
│                 │                     │
│  ┌──────────────▼─────────────────┐  │
│  │  Tool Layer (evaluate-lisp)     │  │
│  └──────────────┬──────────────────┘  │
│                 │                     │
│  ┌──────────────▼─────────────────┐  │
│  │  Evaluator (with error capture) │  │
│  └──────────────┬──────────────────┘  │
│                 │                     │
│  ┌──────────────▼─────────────────┐  │
│  │  Session (persistent state)     │  │
│  └─────────────────────────────────┘  │
└───────────────────────────────────────┘
```

See [Architecture Explanation](docs/explanation/architecture.md) for details.

## Testing

Run the full test suite:

```bash
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(asdf:test-system :cl-mcp-server)"
```

## Project Status

**Version**: 0.1.0

**Status**: Working. Alpa (human testing required).  proof-of-concept. The core functionality is working and tested, but the API may change.

## Contributing

Contributions are welcome! Please:

1. Read [AGENT.md](AGENT.md) for contributor guidelines
2. Review [canon/](canon/) for formal specifications
3. Check open issues for tasks
4. Submit pull requests with tests

## License

MIT License

## Authors

- Abhijit Rao -> quasi (quasi@quasilabs.in)

## Changelog

### Version 0.1.0 (2026-01-22)

**Initial Release**

- MCP protocol implementation (JSON-RPC 2.0 over stdio)
- `evaluate-lisp` tool with persistent session
- Error handling with condition capture and backtraces
- Output stream separation (values, stdout, stderr, warnings)
- Multiple return values support
- Comprehensive test suite (95%+ coverage)
- Canon specification for formal verification
- User documentation and tutorials

---

**Ready to get started?** → [Quickstart Guide](docs/quickstart.md)

**Questions?** → [Documentation](docs/README.md)

**Want to contribute?** → [AGENT.md](AGENT.md)
