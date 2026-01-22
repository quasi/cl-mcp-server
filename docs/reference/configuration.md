# Configuration Reference

**Complete reference for configuring CL-MCP-Server.**

This document covers all configuration options for running CL-MCP-Server in various environments.

## Overview

CL-MCP-Server has minimal configuration requirements. Most behavior is determined by:

1. **System Definition** (`cl-mcp-server.asd`) - Dependencies and build configuration
2. **Startup Script** (`run-server.lisp`) - Server initialization
3. **MCP Client Configuration** - How the client launches the server
4. **Environment Variables** - Runtime configuration (future)

## System Definition

Located in `cl-mcp-server.asd` at the project root.

### System Metadata

```lisp
(asdf:defsystem #:cl-mcp-server
  :description "Model Context Protocol server for Common Lisp evaluation"
  :author "Baba <quasi@quasilabs.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:yason
               #:alexandria
               #:bordeaux-threads
               #:trivial-backtrace)
  :components ...)
```

### Dependencies

| Library | Purpose | Required |
|---------|---------|----------|
| `yason` | JSON parsing/generation | Yes |
| `alexandria` | Utility functions | Yes |
| `bordeaux-threads` | Threading support (future) | Yes |
| `trivial-backtrace` | Portable backtraces | Yes |

**Quicklisp automatically installs dependencies** when you load the system.

### Test System

```lisp
(asdf:defsystem #:cl-mcp-server/tests
  :description "Tests for CL-MCP-Server"
  :depends-on (#:cl-mcp-server
               #:fiveam)
  :components ...)
```

Additional test dependency: `fiveam` (testing framework)

## Startup Script

Located in `run-server.lisp` at the project root.

### Default Configuration

```lisp
#!/usr/bin/env sbcl --script

;;; Load Quicklisp
(load "~/quicklisp/setup.lisp")

;;; Load system
(ql:quickload :cl-mcp-server :silent t)

;;; Start server
(cl-mcp-server:start)
```

### Customization Options

#### Custom Quicklisp Location

If Quicklisp is not in the default location:

```lisp
(load "/custom/path/to/quicklisp/setup.lisp")
```

#### Silent Loading

Suppress Quicklisp output:

```lisp
(ql:quickload :cl-mcp-server :silent t)
```

**Why**: MCP protocol uses stdout, so build output must go to stderr.

#### Redirect Compilation Output

Ensure compilation messages don't interfere with protocol:

```lisp
(let ((*standard-output* *error-output*))
  (ql:quickload :cl-mcp-server))
```

## MCP Client Configuration

### Claude Code

Configuration file: `~/.claude/mcp_config.json`

#### Basic Configuration

```json
{
  "mcpServers": {
    "lisp": {
      "command": "sbcl",
      "args": [
        "--load", "/absolute/path/to/cl-mcp-server/run-server.lisp"
      ]
    }
  }
}
```

**Important**: Use absolute paths, not `~` or relative paths.

#### With Custom SBCL Location

```json
{
  "mcpServers": {
    "lisp": {
      "command": "/usr/local/bin/sbcl",
      "args": [
        "--load", "/absolute/path/to/cl-mcp-server/run-server.lisp"
      ]
    }
  }
}
```

#### Alternative: Direct Script Execution

If `run-server.lisp` has execute permissions and a shebang:

```json
{
  "mcpServers": {
    "lisp": {
      "command": "/absolute/path/to/cl-mcp-server/run-server.lisp",
      "args": []
    }
  }
}
```

**Setup**:
```bash
chmod +x run-server.lisp
# Ensure first line is: #!/usr/bin/env sbcl --script
```

### Generic MCP Client

For any MCP-compatible client:

**Command**: `sbcl --load /path/to/cl-mcp-server/run-server.lisp`
**Transport**: stdio (stdin/stdout)
**Protocol**: JSON-RPC 2.0 over MCP

## Environment Variables

### Current (None Required)

CL-MCP-Server currently doesn't read environment variables.

### Future Configuration Options

Planned environment variable support:

| Variable | Purpose | Default |
|----------|---------|---------|
| `CL_MCP_LOG_LEVEL` | Logging verbosity | `info` |
| `CL_MCP_TIMEOUT` | Evaluation timeout (seconds) | `300` |
| `CL_MCP_MAX_OUTPUT` | Max output size (bytes) | `1048576` |
| `CL_MCP_PACKAGE` | Initial package | `CL-USER` |

**Usage** (future):
```bash
export CL_MCP_LOG_LEVEL=debug
sbcl --load run-server.lisp
```

## Runtime Configuration

### Initial Package

**Default**: `CL-USER`

**Future**: Configurable via startup script:

```lisp
(cl-mcp-server:start :initial-package :my-package)
```

### Output Limits

**Default**: Unlimited (be careful with large outputs)

**Future**: Configurable limits:

```lisp
(cl-mcp-server:start :max-output-size 1048576)  ; 1 MB
```

### Evaluation Timeout

**Current**: No timeout (infinite loops will hang)

**Future**: Configurable timeout:

```lisp
(cl-mcp-server:start :timeout 300)  ; 5 minutes
```

## Advanced Configuration

### Custom System Path

If CL-MCP-Server is not in Quicklisp's path:

```lisp
(push #P"/path/to/cl-mcp-server/" asdf:*central-registry*)
(asdf:load-system :cl-mcp-server)
(cl-mcp-server:start)
```

### Development Mode

Run with source files (not compiled):

```lisp
(asdf:load-system :cl-mcp-server :force t)
(cl-mcp-server:start)
```

### Multiple Servers

**Note**: Each server instance requires its own process.

Run multiple servers for different clients:

```bash
# Terminal 1
sbcl --load run-server.lisp < client1-input > client1-output

# Terminal 2
sbcl --load run-server.lisp < client2-input > client2-output
```

Each server has isolated state.

## Deployment Configurations

### Production

**Goal**: Stable, minimal logging, quick startup

```lisp
#!/usr/bin/env sbcl --script

(load "~/quicklisp/setup.lisp")

;; Silent loading
(let ((*standard-output* *error-output*))
  (ql:quickload :cl-mcp-server :silent t))

;; Start with production settings (future)
(cl-mcp-server:start
  :log-level :warn
  :timeout 600
  :max-output-size 10485760)  ; 10 MB
```

### Development

**Goal**: Verbose logging, quick iteration

```lisp
#!/usr/bin/env sbcl --script

(load "~/quicklisp/setup.lisp")

;; Load with debug info
(declaim (optimize (debug 3) (speed 0)))
(ql:quickload :cl-mcp-server)

;; Start with development settings (future)
(cl-mcp-server:start
  :log-level :debug
  :timeout nil)  ; No timeout
```

### Testing

**Goal**: Isolated, reproducible

```bash
# Use clean environment
env -i sbcl --load run-server.lisp

# Or with explicit configuration
CL_MCP_LOG_LEVEL=debug sbcl --load run-server.lisp
```

## Platform-Specific Configuration

### macOS

**SBCL Installation**:
```bash
brew install sbcl
```

**Quicklisp Location**: `~/quicklisp/`

**Claude Code Config**: `~/.claude/mcp_config.json`

### Linux

**SBCL Installation**:
```bash
# Debian/Ubuntu
apt-get install sbcl

# Fedora
dnf install sbcl

# Arch
pacman -S sbcl
```

**Quicklisp Location**: `~/quicklisp/`

**Claude Code Config**: `~/.config/claude/mcp_config.json` (may vary)

### Windows

**SBCL Installation**: Download from [sbcl.org](http://www.sbcl.org/platform-table.html)

**Quicklisp Location**: `C:\Users\<username>\quicklisp\`

**Run Server**:
```cmd
sbcl --load C:\path\to\cl-mcp-server\run-server.lisp
```

**Claude Code Config**: `%USERPROFILE%\.claude\mcp_config.json`

**Note**: Use forward slashes or escaped backslashes in JSON:
```json
{
  "command": "sbcl",
  "args": ["--load", "C:/path/to/cl-mcp-server/run-server.lisp"]
}
```

## Troubleshooting Configuration

### Server Won't Start

**Check SBCL installation**:
```bash
sbcl --version
```

**Check Quicklisp**:
```bash
sbcl --eval "(ql:quickload :yason)" --quit
```

**Check file paths** in `mcp_config.json` (must be absolute)

### Dependencies Not Found

**Update Quicklisp**:
```bash
sbcl --eval "(ql:update-client)" --quit
sbcl --eval "(ql:update-all-dists)" --quit
```

**Manually install dependencies**:
```bash
sbcl --eval "(ql:quickload '(:yason :alexandria :bordeaux-threads :trivial-backtrace))" --quit
```

### Protocol Errors

**Redirect stderr to debug**:
```bash
sbcl --load run-server.lisp 2>debug.log
```

**Check for output on stdout** (should only be JSON-RPC messages):
```bash
sbcl --load run-server.lisp | tee output.log
```

## Configuration Best Practices

1. **Use Absolute Paths**: In MCP config, always use absolute paths
2. **Silent Loading**: Suppress build output with `:silent t`
3. **Redirect Stderr**: Use `2>logfile` to capture diagnostics
4. **Version Pin**: For production, pin Quicklisp dist version
5. **Test Changes**: Run manually before updating MCP config

## Migration Guide

### From Development to Production

1. **Pin Quicklisp version**:
```lisp
(ql-dist:install-dist
  "http://beta.quicklisp.org/dist/quicklisp/2023-10-21/distinfo.txt"
  :replace t)
```

2. **Add error recovery**:
```lisp
(handler-case
    (cl-mcp-server:start)
  (error (c)
    (format *error-output* "Fatal error: ~A~%" c)
    (quit :unix-status 1)))
```

3. **Add logging**:
```lisp
(defvar *log-file* #P"/var/log/cl-mcp-server.log")
```

## See Also

- [Quickstart](../quickstart.md) - Basic setup instructions
- [Troubleshooting](../how-to/troubleshooting.md) - Common configuration issues
- [MCP Protocol Reference](mcp-protocol.md) - Protocol details
- [Quicklisp Documentation](https://www.quicklisp.org/beta/) - Dependency management
- [ASDF Manual](https://asdf.common-lisp.dev/) - System definition
