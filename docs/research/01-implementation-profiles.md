# Implementation Profiles: Common Lisp MCP Servers

## Overview

This document profiles five implementations of MCP (Model Context Protocol) servers related to Common Lisp, plus our own cl-mcp-server as a baseline.

---

## 1. belyak/mcp-srv-lisp

**Repository**: https://github.com/belyak/mcp-srv-lisp

### Basic Facts

| Attribute | Value |
|-----------|-------|
| License | Apache-2.0 |
| Author | Andrei Beliak |
| Version | 0.1.0 |
| Primary Language | Common Lisp |
| JSON-RPC Version | 2.0 |
| Protocol Version | 2024-11-05 |

### Dependencies (13 total)

- alexandria, serapeum (utilities)
- yason (JSON)
- bordeaux-threads, usocket (networking/concurrency)
- flexi-streams, local-time (stream/time)
- quri, cl-ppcre (URI/regex)
- unix-opts, trivial-signal (CLI/signals)
- closer-mop, split-sequence (metaprogramming)

### Architecture

**Modular Package Structure**:
- `mcp-server.constants` - Protocol/version constants
- `mcp-server.types` - JSON-RPC types
- `mcp-server.templates` - Tool/prompt/resource templates
- `mcp-server.tools` - Tool registration and execution
- `mcp-server.prompts` - Prompt management
- `mcp-server.resources` - Resource handling
- `mcp-server.main` - Server startup and dispatch

**Transport**: stdio (NDJSON)

**Tool System**:
- Template-based definitions (JSON strings parsed at runtime)
- Hash-table registry with caching
- Command pattern for tool execution via `mcp-tool` class
- Generic functions: `execute-tool`, `validate-params`, `tool-category`

### MCP Capabilities

| Capability | Status |
|------------|--------|
| Tools | Implemented (template-based) |
| Resources | Implemented (file:// URIs) |
| Prompts | Implemented (with arguments) |

### Notable Design Decisions

1. **Template-Driven Configuration**: Tools, prompts, and resources defined as JSON templates in `*-template*` variables
2. **Class Hierarchy**: Uses CLOS extensively with `json-serializable`, `mcp-named-entity`, `mcp-identifiable`
3. **Factory Pattern**: `*request-factory*`, `*tool-factory*` for object creation
4. **Parameter Validation**: Custom conditions for missing/invalid parameters

### Strengths

- Complete MCP feature set (tools, resources, prompts)
- Well-organized modular architecture
- Comprehensive type system with CLOS

### Weaknesses

- No code evaluation tool (only a `get_current_time_in_city` example)
- No sandboxing mechanisms
- Template approach limits runtime tool registration

---

## 2. gornskew/lisply-mcp

**Repository**: https://github.com/gornskew/lisply-mcp

### Basic Facts

| Attribute | Value |
|-----------|-------|
| License | AGPL-3.0 |
| Author | Dave Cooper (Genworks) |
| Primary Language | **Node.js (JavaScript)** |
| Stars | 44 |
| Last Updated | January 2026 |

**Important**: Despite the name, this is NOT a Common Lisp MCP server. It's a Node.js middleware that bridges MCP clients to Lisp backends via HTTP.

### Architecture

```
MCP Client (Claude Desktop)
    │ stdio JSON-RPC
    ▼
Node.js Wrapper (mcp-wrapper.js)
    │ HTTP POST/GET
    ▼
Lisply Backend (containerized Lisp - Gendl)
```

### Dependencies

- `commander` ^11.0.0 (CLI parsing)
- External: Docker containers running Gendl/Common Lisp

### MCP Capabilities

| Capability | Status |
|------------|--------|
| Tools | Primary feature (lisp_eval, http_request, ping, docs) |
| Resources | Returns empty |
| Prompts | Returns empty |

### Notable Design Decisions

1. **"Lisply" Protocol**: Custom intermediate protocol between MCP and Lisp backends
2. **Tool Name Prefixing**: `gendl-ccl__lisp_eval` enables multiple backend namespaces
3. **Docker-First**: All Lisp execution in containers
4. **HTTP-Only Communication**: Removed Docker exec for security

### Strengths

- Mature Docker integration
- Backend-agnostic design
- Production-ready error handling and logging
- Actively maintained

### Weaknesses

- Not actually a Lisp implementation
- Limited MCP features (only tools meaningful)
- Requires Docker infrastructure
- AGPL license may limit commercial use

---

## 3. 40ants/mcp

**Repository**: https://github.com/40ants/mcp

### Basic Facts

| Attribute | Value |
|-----------|-------|
| License | BSD-style (40ants standard) |
| Author | Alexander Artemenko |
| Version | 0.3.0 |
| Primary Language | Common Lisp |
| Protocol Version | 2024-11-05 |

### Dependencies

- alexandria, sse-server, trivial-gray-streams, uuid, yason
- Built on: 40ants/openrpc framework
- Full stack: Clack -> Hunchentoot for HTTP

### Architecture

**Transport Abstraction**:
```lisp
;; Base protocol (src/transport/base.lisp)
(defgeneric start-loop (transport message-handler))
(defgeneric stop-loop (transport))
(defgeneric receive-message (transport))
(defgeneric send-message (transport message))
```

**Two Transports**:
1. **STDIO**: Default for local subprocess usage
2. **HTTP/SSE**: Streamable HTTP for remote/cloud deployment

**Tool Definition**:
```lisp
(define-tool (example-tools echo) (text)
  (:summary "Returns input text back")
  (:param text string "Text to echo")
  (:result (soft-list-of text-content))
  (list (make-instance 'text-content :text text)))
```

### MCP Capabilities

| Capability | Status |
|------------|--------|
| Tools | Full implementation with JSON Schema |
| Resources | Not implemented |
| Prompts | Not implemented |

### Notable Design Decisions

1. **OpenRPC Foundation**: Built on 40ants/openrpc for JSON-RPC handling
2. **Declarative Tool Definition**: `define-tool` macro with type annotations
3. **SLYNK Integration**: Optional remote debugging via SLYNK_PORT
4. **Pagination Support**: `tools-list-response-with-cursor` for large tool sets

### Strengths

- Clean transport abstraction
- Professional library design
- Active maintenance (recent commits)
- Good for framework building

### Weaknesses

- Only implements tools (no resources/prompts)
- No built-in code evaluation
- Requires multiple 40ants ecosystem libraries
- HTTP mode requires patches for Cloud Run binding

---

## 4. cl-ai-project/cl-mcp

**Repository**: https://github.com/cl-ai-project/cl-mcp

### Basic Facts

| Attribute | Value |
|-----------|-------|
| License | MIT |
| Authors | cxxxr, Satoshi Imai |
| Primary Language | Common Lisp (SBCL 2.x) |
| Last Updated | January 2026 (very active) |

### Dependencies

- alexandria, cl-ppcre, yason, usocket, bordeaux-threads
- eclector (CST parsing for Lisp-aware editing)
- hunchentoot (HTTP), rove (testing)

### Architecture

**Three Transports**:
1. **STDIO**: For subprocess integration
2. **TCP**: Socket-based communication
3. **HTTP**: RESTful API via Hunchentoot

**Tool Definition Macro**:
```lisp
(define-tool name
  :description "Schema documentation"
  :args ((path :type :string :required t :description "..."))
  :body (...implementation...))
```

### MCP Capabilities

| Capability | Status |
|------------|--------|
| Tools | Extensive (15+ tools) |
| Resources | Not documented |
| Prompts | Agent guidance documents |

### Built-in Tools

| Tool | Description |
|------|-------------|
| `repl-eval` | Execute Lisp expressions with package context |
| `fs-list-directory` | Project-scoped directory listing |
| `fs-read-file` | Read files with hidden file filtering |
| `fs-write-file` | Write to project-relative paths |
| `lisp-read-file` | Collapsed/expanded Lisp file viewing |
| `lisp-edit-form` | Structure-aware editing via Eclector CST |
| `lisp-check-parens` | Delimiter mismatch detection |
| `clgrep-search` | Lisp-aware code search |
| `code-find` / `code-describe` | Symbol introspection (sb-introspect) |
| `clhs-lookup` | HyperSpec reference |
| `run-tests` | Unified test runner (Rove/ASDF) |
| `inspect-object` | Runtime object inspection |

### Security Mechanisms

1. **Project Root Restriction**: File operations limited to project root + registered ASDF paths
2. **Path Validation**: `allowed-read-path-p`, `ensure-write-path`
3. **Hidden File Filtering**: Excludes `.git`, `.cache`, compiled files
4. **Audit Logging**: JSON logs with path, bytes, FD count

### Notable Design Decisions

1. **Agent-Oriented Development**: Includes `prompts/repl-driven-development.md` for AI guidance
2. **Structure-Aware Editing**: Uses Eclector CST for surgical Lisp modifications
3. **Parinfer Integration**: Auto-fixes unbalanced parentheses
4. **REPL-First Workflow**: EXPLORE → EXPERIMENT → PERSIST → VERIFY

### Strengths

- Most feature-complete implementation
- Strong security model
- Lisp-aware tooling (not just text)
- Comprehensive agent guidance

### Weaknesses

- SBCL-specific (sb-introspect, etc.)
- Complex dependency tree
- No sandboxing for code execution
- Some tools expose internal implementation details

---

## 5. xbill999 Medium Article (mcp-https-lisp)

**Source**: https://xbill999.medium.com/mcp-development-with-lisp-cloud-run-and-gemini-cli-c8e9343e6d05
**Repository**: https://github.com/xbill9/gemini-cli-codeassist

### Basic Facts

| Attribute | Value |
|-----------|-------|
| Author | xbill (xbill999) |
| Primary Focus | Cloud Run deployment tutorial |
| MCP Framework | 40ants-mcp (patched) |
| Target Client | Gemini CLI |

### Architecture

Two implementations:
1. **mcp-stdio-lisp**: 6 tools for local development
2. **mcp-https-lisp**: 2 tools for Cloud Run deployment

### Example Tools

| Tool | Description |
|------|-------------|
| `get-system-info` | CL implementation info |
| `get-lisp-koan` | Random Lisp wisdom |
| `symbolic-differentiate` | Symbolic AI demonstration |

### Key Insights

**Monkey Patching Required**: The article highlights significant patches needed for:
- JSON-RPC field casing (`is_error` → `isError`)
- SSE header handling
- Server binding to `0.0.0.0`

**Dependency Fragility**: "Patches depend on private/internal symbols... any update to 40ants-mcp could silently break the server"

### Lessons

1. 40ants-mcp requires customization for strict MCP compliance
2. STDIO transport needs clean stdout (logging to stderr)
3. Use structured JSON logging for cloud observability

---

## 6. Our Implementation: cl-mcp-server

**Repository**: Local (this project)

### Basic Facts

| Attribute | Value |
|-----------|-------|
| License | MIT |
| Primary Language | Common Lisp (SBCL) |
| Protocol Version | 2025-06-18 |
| Transport | NDJSON over stdio |

### Dependencies (4 runtime)

- yason (JSON)
- alexandria (utilities)
- bordeaux-threads (future use)
- trivial-backtrace (portable backtraces)

### Architecture

```
┌────────────────────────────────┐
│ MCP Protocol (server.lisp)     │
├────────────────────────────────┤
│ JSON-RPC (json-rpc.lisp)       │
├────────────────────────────────┤
│ Transport (transport.lisp)     │
├────────────────────────────────┤
│ Tools (tools.lisp)             │
├────────────────────────────────┤
│ Session (session.lisp)         │
├────────────────────────────────┤
│ Evaluator (evaluator.lisp)     │
└────────────────────────────────┘
```

### MCP Capabilities

| Capability | Status |
|------------|--------|
| Tools | 4 built-in (evaluate-lisp, list-definitions, reset-session, load-system) |
| Resources | Not implemented |
| Prompts | Not implemented |

### Built-in Tools

| Tool | Description |
|------|-------------|
| `evaluate-lisp` | Execute CL code with stream capture |
| `list-definitions` | Session introspection |
| `reset-session` | Clear tracked definitions |
| `load-system` | ASDF system loading |

### Design Decisions

1. **REPL-Like Session**: Single persistent session per server
2. **Definition Tracking**: Explicit detection of `defun`, `defvar`, etc.
3. **Muffle Warnings**: Capture warnings but continue evaluation
4. **No Sandboxing**: Full Lisp access by design

### Strengths

- Minimal dependencies
- Clean layered architecture
- Comprehensive error handling
- Complete Canon specification

### Weaknesses

- Single-threaded
- No sandboxing
- Limited to tools (no resources/prompts)
- SBCL-focused
