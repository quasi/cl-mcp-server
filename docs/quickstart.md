# Quickstart: Get Running in 5 Minutes

Get your CL-MCP-Server up and running quickly.

## Prerequisites

- SBCL (Steel Bank Common Lisp) installed
- Quicklisp installed and configured
- Claude Code CLI or other MCP client

## Installation

### 1. Clone or Download

```bash
cd ~/projects
git clone <repository-url> cl-mcp-server
cd cl-mcp-server
```

### 2. Load the System

```bash
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)"
```

This will:
- Load all dependencies
- Compile the source files
- Prepare the server for execution

## Running the Server

### Start the Server

```bash
./run-server.lisp
```

Or manually:

```bash
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)" \
     --eval "(cl-mcp-server:start)"
```

The server will:
- Initialize the MCP protocol handler
- Set up stdio transport (stdin/stdout)
- Create a persistent REPL session
- Wait for requests from Claude

## Testing the Connection

### Configure Claude Code

Add to your Claude Code configuration (location varies by platform):

```json
{
  "mcpServers": {
    "lisp": {
      "command": "/path/to/cl-mcp-server/run-server.lisp"
    }
  }
}
```

### Send Your First Evaluation

In Claude Code:

```
Please evaluate (+ 1 2 3) in Lisp
```

Expected response:

```
=> 6
```

## Quick Examples

### Basic Arithmetic

```lisp
(* 7 6)
```

Result: `=> 42`

### Define a Function

```lisp
(defun greet (name)
  (format nil "Hello, ~A!" name))
```

Result: `=> GREET`

### Use the Function

```lisp
(greet "World")
```

Result: `=> "Hello, World!"`

**Note**: The function persists in the session!

### List Processing

```lisp
(mapcar #'1+ '(1 2 3 4 5))
```

Result: `=> (2 3 4 5 6)`

## What's Next?

- **Learn more**: See the [Tutorial: Your First REPL Session](tutorials/01-first-session.md)
- **Understand concepts**: Read [Explanation: How It Works](explanation/architecture.md)
- **Solve problems**: Browse [How-To Guides](how-to/)
- **API reference**: See [Reference: evaluate-lisp Tool](reference/evaluate-lisp.md)

## Troubleshooting

### Server won't start

**Problem**: SBCL not found.

**Solution**: Ensure SBCL is in your PATH:
```bash
which sbcl
```

### No response from Claude

**Problem**: MCP configuration incorrect.

**Solution**: Verify the `command` path in your MCP config points to the correct location.

### Functions don't persist

**Problem**: Session isn't maintaining state.

**Solution**: Check that you're using the same Claude Code session - each new conversation creates a new server instance.

---

**You're ready to go!** Start evaluating Lisp code with Claude.
