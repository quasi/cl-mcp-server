# Research: Common Lisp MCP Implementations

Comparative study of Common Lisp MCP server implementations to inform the future direction of cl-mcp-server.

## Documents

| Document | Description |
|----------|-------------|
| [00-decision-context.md](00-decision-context.md) | Research framing, constraints, and comparison dimensions |
| [01-implementation-profiles.md](01-implementation-profiles.md) | Detailed profiles of 6 implementations |
| [02-comparative-analysis.md](02-comparative-analysis.md) | Trade-off analysis and gap identification |
| [03-recommendations.md](03-recommendations.md) | Recommended direction with cost-benefit analysis |

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

### Critical Gaps
1. **No security model** - Need project-scoped file access
2. **No file tools** - Can't read/write project files
3. **No search tools** - Can't search codebase

### Recommended Direction
**Agent-Optimized REPL Server** - Focus on what makes us unique (live Lisp image) rather than full MCP compliance.

## Research Date

January 2026
