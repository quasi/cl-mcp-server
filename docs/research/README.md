# Research: Common Lisp MCP Implementations

Comparative study of Common Lisp MCP server implementations to inform the future direction of cl-mcp-server.

## Documents

| Document | Description |
|----------|-------------|
| [00-decision-context.md](00-decision-context.md) | Research framing, constraints, and comparison dimensions |
| [01-implementation-profiles.md](01-implementation-profiles.md) | Detailed profiles of 6 implementations |
| [02-comparative-analysis.md](02-comparative-analysis.md) | Trade-off analysis and gap identification |
| [03-recommendations.md](03-recommendations.md) | Recommended direction with cost-benefit analysis |
| [04-sly-lessons-for-mcp.md](04-sly-lessons-for-mcp.md) | Lessons from Sly/SLYNK for cl-mcp-server |
| [05-implementation-roadmap.md](05-implementation-roadmap.md) | Concrete implementation plan with phases |

## Implementations Studied

| Implementation | Type | Key Characteristic |
|----------------|------|-------------------|
| [belyak/mcp-srv-lisp](https://github.com/belyak/mcp-srv-lisp) | Pure CL | Complete MCP features (tools, resources, prompts) |
| [gornskew/lisply-mcp](https://github.com/gornskew/lisply-mcp) | Node.js proxy | Docker-based Lisp backend integration |
| [40ants/mcp](https://github.com/40ants/mcp) | Pure CL | Transport abstraction framework |
| [cl-ai-project/cl-mcp](https://github.com/cl-ai-project/cl-mcp) | Pure CL | Most feature-rich, agent-optimized |
| xbill999/mcp-https-lisp | Pure CL | Cloud Run deployment example |
| cl-mcp-server (ours) | Pure CL | Session persistence, definition tracking |

## Key Findings

### Our Unique Strengths
1. **Definition tracking** - No competitor tracks session definitions
2. **Minimal dependencies** - 4 vs 8-13 in competitors
3. **Clean architecture** - Well-separated layers
4. **Modern protocol** - MCP 2025-06-18

### Clarified Scope (Not Gaps)

Claude has native file tools (Read, Write, Edit, Grep, Glob). The MCP server provides **live image access**, not file access:

| Claude Has | MCP Server Provides |
|------------|-------------------|
| File read/write | Code evaluation |
| Text search | Symbol introspection |
| Directory listing | ASDF/Quicklisp loading |

### Actual Development Priorities
1. **Introspection tools** - describe-symbol, who-calls, apropos
2. **Structured errors** - Rich error data for intelligent debugging
3. **ASDF/Quicklisp integration** - Load systems, query dependencies

### Recommended Direction
**Agent-Optimized REPL Server** - Focus on what makes us unique (live Lisp image) rather than full MCP compliance.

### Sly/SLYNK Lessons (New)

The second research phase analyzed Sly (gold standard for human CL development) and agent-q (AI agent built on Sly):

1. **The image is truth** - Let Claude ask the image via introspection, not guess from text
2. **Introspection before evaluation** - Claude should understand before modifying
3. **Structured errors** - Rich error data enables intelligent recovery
4. **SBCL is fine** - Use `sb-introspect` capabilities freely
5. **Many small tools** - Better for LLM tool use patterns

### Priority Tools to Add

| Tool | Purpose | Sly Equivalent |
|------|---------|----------------|
| describe-symbol | Full symbol info | `C-c C-d d` |
| apropos-search | Find symbols | `C-c C-d a` |
| who-calls | Find callers | `C-c C-w c` |
| macroexpand-form | Understand macros | `C-c C-m` |
| get-backtrace | Debug errors | Debugger buffer |

## Research Date

January 2026
