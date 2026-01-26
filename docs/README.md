# CL-MCP-Server Documentation

**Welcome to the CL-MCP-Server documentation!**

This is a Model Context Protocol (MCP) server that enables Claude to evaluate Common Lisp code in a persistent REPL session.

## Documentation Structure

This documentation follows the [Diátaxis framework](https://diataxis.fr/) for clarity and discoverability.

### 🚀 Getting Started

**New to CL-MCP-Server? Start here:**

- **[Quickstart](quickstart.md)** - Get running in 5 minutes
- **[Tutorial: First REPL Session](tutorials/01-first-session.md)** - Learn by building a temperature converter

### 📚 Tutorials

**Learning-oriented guides** that teach by doing:

- [Your First REPL Session](tutorials/01-first-session.md) - Build a temperature converter incrementally

### 🔧 How-To Guides

**Task-oriented guides** for solving specific problems:

- [How to Explore Code](how-to/explore-code.md) - Use introspection tools to understand Lisp code
- [How to Switch Packages](how-to/switch-packages.md) - Work in different package namespaces
- [How to Load Quicklisp Systems](how-to/load-quicklisp-systems.md) - Use Common Lisp libraries
- [How to Validate Before Save](how-to/validate-before-save.md) - Check syntax before writing files
- [How to Handle Errors Gracefully](how-to/handle-errors-gracefully.md) - Write robust error-handling code
- [Troubleshooting Common Issues](how-to/troubleshooting.md) - Diagnose and fix problems

### 💡 Explanation

**Understanding-oriented articles** that build conceptual knowledge:

- [Architecture](explanation/architecture.md) - How CL-MCP-Server works under the hood
- [Why Persistent Sessions?](explanation/persistent-sessions.md) - Design rationale for stateful REPL
- [The Lisp Condition System](explanation/condition-system.md) - Understanding error handling

### 📖 Reference

**Information-oriented specifications** for lookup:

- [evaluate-lisp Tool](reference/evaluate-lisp.md) - Complete API specification
- [Introspection Tools](reference/introspection-tools.md) - Code exploration and analysis
- [CLOS Tools](reference/clos-tools.md) - Object-oriented programming support
- [ASDF & Quicklisp](reference/asdf-quicklisp.md) - Loading libraries and systems
- [Profiling Tools](reference/profiling-tools.md) - Performance analysis
- [MCP Protocol Details](reference/mcp-protocol.md) - JSON-RPC wire protocol
- [Configuration Options](reference/configuration.md) - Setup and deployment

## Quick Links

| I want to... | Go to... |
|--------------|----------|
| Get started immediately | [Quickstart](quickstart.md) |
| Learn interactively | [First REPL Session Tutorial](tutorials/01-first-session.md) |
| Understand the architecture | [Architecture Explanation](explanation/architecture.md) |
| Look up API details | [evaluate-lisp Reference](reference/evaluate-lisp.md) |
| See the formal specification | `../canon/` directory |

## For Different Audiences

### For End Users

**Goal**: Use CL-MCP-Server with Claude to evaluate Lisp code.

**Path**:
1. [Quickstart](quickstart.md) - Set it up
2. [First REPL Session Tutorial](tutorials/01-first-session.md) - Learn by doing
3. [evaluate-lisp Reference](reference/evaluate-lisp.md) - When you need details

### For Contributing Developers

**Goal**: Modify or extend the CL-MCP-Server codebase.

**Path**:
1. Read `../AGENT.md` - Contributor instructions
2. Read `../canon/INDEX.md` - Formal specifications
3. [Architecture Explanation](explanation/architecture.md) - Understand the design

### For External Agents

**Goal**: Integrate with CL-MCP-Server programmatically.

**Path**:
- Read formal specifications in `../canon/features/` (PUBLIC artifacts only)
- See `../canon/features/mcp-protocol/` for protocol details
- See `../canon/features/code-evaluator/contracts/evaluate-lisp-tool.md` for tool spec

## What is MCP?

**Model Context Protocol (MCP)** is a standard protocol for AI agents (like Claude) to interact with external services.

Key concepts:
- **Server**: Provides capabilities (like evaluating Lisp code)
- **Client**: Consumes capabilities (like Claude Code)
- **Tools**: Specific operations (like `evaluate-lisp`)
- **Transport**: Communication channel (stdio in this case)

**Learn more**: [MCP Documentation](https://modelcontextprotocol.io/)

## What is a REPL?

**REPL** = **R**ead-**E**val-**P**rint **L**oop

A REPL is an interactive programming environment:
1. **Read** code from the user
2. **Eval**uate it
3. **Print** the result
4. **Loop** back to step 1

CL-MCP-Server provides REPL-like interaction through Claude:
- Definitions persist across evaluations
- State accumulates (functions, variables)
- Incremental development is natural

## Features

### Core Capabilities
- ✓ **Persistent Session**: Functions and variables persist across requests
- ✓ **Rich Output**: Separates return values, stdout, stderr, and warnings
- ✓ **Error Handling**: Graceful condition handling with backtraces
- ✓ **Multiple Values**: Full support for Common Lisp's multiple return values
- ✓ **Stream Capture**: Captures all output streams during evaluation
- ✓ **Safety**: Server never crashes due to user code errors
- ✓ **Standard Protocol**: Uses MCP and JSON-RPC 2.0

### Advanced Tools
- ✓ **Code Introspection**: Explore symbols, functions, and cross-references
- ✓ **CLOS Intelligence**: Inspect classes, slots, and inheritance hierarchies
- ✓ **Syntax Validation**: Verify code correctness before execution
- ✓ **Macro Expansion**: Understand macro transformations
- ✓ **Library Loading**: Automatic dependency management via Quicklisp
- ✓ **Performance Profiling**: Statistical profiling for CPU, time, and memory
- ✓ **Compilation Tools**: Compile and analyze code without executing

## Examples

### Basic Evaluation

```
User: Please evaluate (+ 1 2 3)

Response: => 6
```

### Persistent Definitions

```
User: Please evaluate (defun square (x) (* x x))

Response: => SQUARE

User: Please evaluate (square 7)

Response: => 49
```

### With Output

```
User: Please evaluate (progn (print "Hello") 42)

Response:
[stdout]
"Hello"

=> 42
```

### Error Handling

```
User: Please evaluate (/ 1 0)

Response:
[ERROR] DIVISION-BY-ZERO
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).

[Backtrace]
0: (/ 1 0)
```

## System Requirements

- **Lisp Implementation**: SBCL (Steel Bank Common Lisp)
- **Dependency Manager**: Quicklisp
- **MCP Client**: Claude Code or compatible client

## Project Status

**Version**: 0.1.0 (draft)

**Status**: Early development, proof-of-concept.

**Features**:
- ✓ MCP protocol implementation
- ✓ Code evaluation
- ✓ Session persistence
- ✓ Error handling
- ⧗ Advanced features (pending)

## Getting Help

- **Issues**: Report bugs or request features at the project repository
- **Documentation bugs**: Found an error in these docs? Let us know!
- **Questions**: For usage questions, see the [How-To Guides](#how-to-guides)

## Contributing

Interested in contributing?

1. Read `../AGENT.md` for contributor guidelines
2. Review `../canon/` for formal specifications
3. See open issues in the issue tracker

## License

See LICENSE file in the project root.

---

**Ready to get started?** → [Quickstart Guide](quickstart.md)

**Want to understand deeply?** → [Architecture Explanation](explanation/architecture.md)

**Need API details?** → [evaluate-lisp Reference](reference/evaluate-lisp.md)
