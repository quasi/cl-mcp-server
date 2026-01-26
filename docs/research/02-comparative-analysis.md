# Comparative Analysis: CL MCP Implementations

## Executive Summary

This analysis compares five Common Lisp MCP implementations against our cl-mcp-server to inform future development direction. The key finding is that **no existing implementation fully serves our core aim**: empowering Claude with a running Lisp image for interactive development.

| Implementation | Pure CL | Code Eval | Security | Agent-Optimized |
|----------------|---------|-----------|----------|-----------------|
| belyak/mcp-srv-lisp | Yes | No | No | No |
| gornskew/lisply-mcp | **No (Node.js)** | Yes (via HTTP) | Docker | Partial |
| 40ants/mcp | Yes | No | No | No |
| cl-ai-project/cl-mcp | Yes | Yes | **Project-scoped** | **Yes** |
| xbill999/mcp-https-lisp | Yes | No | No | No |
| **cl-mcp-server** | Yes | **Yes** | No | Partial |

---

## Dimension-by-Dimension Comparison

### 1. Protocol Implementation

| Implementation | JSON-RPC | MCP Version | Transport |
|----------------|----------|-------------|-----------|
| belyak/mcp-srv-lisp | 2.0 | 2024-11-05 | stdio |
| gornskew/lisply-mcp | 2.0 | ~0.1.0 | stdio→HTTP proxy |
| 40ants/mcp | 2.0 | 2024-11-05 | stdio, HTTP/SSE |
| cl-ai-project/cl-mcp | 2.0 | Not specified | stdio, TCP, HTTP |
| xbill999/mcp-https-lisp | 2.0 | 2024-11-05 | stdio, HTTP |
| **cl-mcp-server** | 2.0 | 2025-06-18 | stdio |

**Analysis**: Our implementation uses the newest protocol version (2025-06-18). The 40ants/mcp framework offers the best transport abstraction if we need HTTP/SSE in the future.

### 2. MCP Capabilities Matrix

| Implementation | Tools | Resources | Prompts | Sampling |
|----------------|-------|-----------|---------|----------|
| belyak/mcp-srv-lisp | ✓ | ✓ | ✓ | - |
| gornskew/lisply-mcp | ✓ | - | - | - |
| 40ants/mcp | ✓ | - | - | - |
| cl-ai-project/cl-mcp | ✓✓ | - | ✓ (docs) | - |
| xbill999/mcp-https-lisp | ✓ | - | - | - |
| **cl-mcp-server** | ✓ | - | - | - |

**Analysis**: belyak/mcp-srv-lisp has the most complete MCP feature coverage, but lacks code evaluation. cl-ai-project/cl-mcp has the most tools but frames prompts as agent guidance documents rather than MCP prompts.

### 3. Code Evaluation Features

| Implementation | Eval | Session Persist | Definition Track | Output Capture |
|----------------|------|-----------------|------------------|----------------|
| belyak/mcp-srv-lisp | - | - | - | - |
| gornskew/lisply-mcp | ✓ | Via backend | - | ✓ |
| 40ants/mcp | - | - | - | - |
| cl-ai-project/cl-mcp | ✓ | ✓ | - | ✓ |
| xbill999/mcp-https-lisp | - | - | - | - |
| **cl-mcp-server** | ✓ | ✓ | ✓ | ✓ |

**Analysis**: **cl-mcp-server has the richest code evaluation model** with definition tracking and session persistence. cl-ai-project/cl-mcp is the closest competitor but lacks definition tracking.

### 4. Security Model

| Implementation | Sandboxing | Path Restriction | Timeouts | Resource Limits |
|----------------|------------|------------------|----------|-----------------|
| belyak/mcp-srv-lisp | None | None | - | - |
| gornskew/lisply-mcp | Docker | Container-level | ✓ | Docker limits |
| 40ants/mcp | None | None | - | - |
| cl-ai-project/cl-mcp | **Project-scoped** | **Yes** | ✓ | - |
| xbill999/mcp-https-lisp | None | None | - | - |
| **cl-mcp-server** | None | None | - | - |

**Analysis**: **cl-ai-project/cl-mcp has the strongest security model** with project-scoped file access, path validation, and hidden file filtering. This is a significant gap in our implementation.

### 5. Tool Richness

| Implementation | Tools | Categories |
|----------------|-------|------------|
| belyak/mcp-srv-lisp | 1 | Time |
| gornskew/lisply-mcp | 5 | Eval, HTTP, Docs, Search |
| 40ants/mcp | 2 (example) | Echo, Add |
| cl-ai-project/cl-mcp | **15+** | Eval, FS, Search, Edit, Introspect, Test |
| xbill999/mcp-https-lisp | 6 | Info, Koan, Symbolic |
| **cl-mcp-server** | 4 | Eval, Session, System |

**Analysis**: cl-ai-project/cl-mcp is far ahead in tool richness. Their Lisp-aware tools (lisp-edit-form, lisp-check-parens, clgrep-search) are particularly valuable for agent-assisted development.

### 6. Dependencies

| Implementation | Runtime Deps | Test Deps | Ecosystem Lock-in |
|----------------|--------------|-----------|-------------------|
| belyak/mcp-srv-lisp | 13 | - | Medium (utilities) |
| gornskew/lisply-mcp | 1 (JS) | - | High (Docker+Gendl) |
| 40ants/mcp | 5+ | - | High (40ants stack) |
| cl-ai-project/cl-mcp | 8 | rove | Low |
| xbill999/mcp-https-lisp | 8+ | - | High (40ants patches) |
| **cl-mcp-server** | **4** | fiveam | **Low** |

**Analysis**: cl-mcp-server has the leanest dependency tree. This is a competitive advantage for adoption and maintenance.

---

## Trade-off Analysis

### Trade-off 1: Feature Richness vs. Simplicity

```
MORE FEATURES                              SIMPLER
     │                                         │
     ▼                                         ▼
cl-ai-project ─── belyak ─── 40ants ─── cl-mcp-server
```

**cl-ai-project/cl-mcp** maximizes features at the cost of complexity and SBCL-specificity.
**cl-mcp-server** prioritizes simplicity but may lack features agents need.

**Decision Point**: Do we add features incrementally or adopt patterns from cl-ai-project?

### Trade-off 2: Security vs. Expressiveness

```
MORE SECURE                               MORE EXPRESSIVE
     │                                            │
     ▼                                            ▼
lisply (Docker) ─── cl-ai-project ─── cl-mcp-server
```

**lisply-mcp** achieves security through containerization (architectural isolation).
**cl-ai-project/cl-mcp** uses project-scoping (application-level restriction).
**cl-mcp-server** trusts the user completely.

**Decision Point**: Is project-scoping sufficient, or do we need sandboxing?

### Trade-off 3: Pure Lisp vs. Hybrid Architecture

```
PURE LISP                                      HYBRID
     │                                            │
     ▼                                            ▼
cl-mcp-server ─── 40ants ─── belyak ─── lisply-mcp
```

**Pure Lisp implementations** keep everything in one runtime but lose isolation.
**Hybrid approaches** (lisply-mcp) add complexity but enable Docker-based security.

**Decision Point**: Should we remain pure Lisp or consider a supervisor process?

### Trade-off 4: MCP Compliance vs. Lisp Idioms

The 40ants ecosystem struggles with MCP compliance due to Lisp conventions:
- Field naming: `is_error` vs `isError`
- JSON null handling
- Required patching for strict compliance

**cl-mcp-server** handles this in `convert-for-json` explicitly.

**Decision Point**: Is our current approach robust enough for all MCP clients?

---

## Gap Analysis: cl-mcp-server

### Re-evaluation: What Claude Already Has

**Critical insight**: Claude natively has file system access through its own tools (Read, Write, Edit, Grep, Glob). The MCP server does NOT need to duplicate these capabilities.

| Capability | Claude's Native Tools | MCP Server Needed? |
|------------|----------------------|-------------------|
| Read files | Read tool | **No** |
| Write files | Write, Edit tools | **No** |
| Search files | Grep, Glob tools | **No** |
| Directory listing | Bash (ls) | **No** |
| **Live evaluation** | None | **Yes** |
| **Image introspection** | None | **Yes** |
| **ASDF/Quicklisp loading** | None | **Yes** |

### Actual Gaps (Should Address)

| Gap | Impact | Notes |
|-----|--------|-------|
| Limited introspection | Claude can't query symbol info, callers, etc. | Phase A priority |
| Basic error reporting | Errors are strings, not structured data | Phase C priority |
| No CLOS inspection | Can't understand class hierarchies | Phase D priority |
| Limited ASDF integration | Only `load-system`, no dependency info | Phase E priority |

### Not Gaps (Previously Misidentified)

| "Gap" | Why It's Not a Gap |
|-------|-------------------|
| No file tools | Claude has Read, Write, Edit tools natively |
| No search tools | Claude has Grep, Glob tools natively |
| No security model | Trust model: Claude is trusted; MCP provides image access, not file access |
| No resources support | File content exposure via MCP is redundant - Claude reads files directly |

### Low Priority Considerations

| Item | Impact | Notes |
|------|--------|-------|
| HTTP transport | Deployment flexibility | STDIO sufficient for local Claude Code |
| HyperSpec lookup | Convenience | Claude can web search |
| Test runner | Automation | Can use `evaluate-lisp` with test forms |

---

## Unique Strengths: cl-mcp-server

Despite gaps, our implementation has distinct advantages:

### 1. Definition Tracking

**No other implementation tracks what the user defines in the session.** This enables:
- `list-definitions` tool for session introspection
- Proper cleanup on `reset-session`
- Potential for undo/rollback features

### 2. Clean Layered Architecture

Our separation of concerns (transport → JSON-RPC → protocol → tools → session → evaluator) is cleaner than competitors.

### 3. Minimal Dependencies

4 runtime dependencies vs. 8-13 in competitors reduces:
- Installation friction
- Dependency conflicts
- Maintenance burden

### 4. Formal Specification

The Canon specification provides:
- Clear contracts for future development
- Test scenarios for verification
- Architecture documentation

### 5. Modern Protocol Version

Using MCP 2025-06-18 ensures compatibility with latest Claude features.

---

## Patterns to Adopt

### From cl-ai-project/cl-mcp

1. **Rich Introspection Tools**
   - Symbol description with full metadata
   - Cross-reference tools (who-calls, who-references)
   - CLOS class inspection

2. **Structured Error Data**
   - Condition type classification
   - Backtrace with source locations
   - Available restarts

### From 40ants/mcp

1. **Declarative Tool Definition**

   ```lisp
   (define-tool (api name) (args...)
     (:summary "...")
     (:param arg type "...")
     body)
   ```

   Reduces boilerplate for adding new tools.

### From Agent-Q (Sly-based)

1. **sb-introspect Integration**
   ```lisp
   (sb-introspect:function-lambda-list fn)
   (sb-introspect:who-calls sym)
   (sb-introspect:find-definition-source sym)
   ```

2. **Tool Safety Levels**
   - `:safe` - Read-only introspection
   - `:moderate` - State-changing evaluation
   - `:dangerous` - Potentially destructive operations

### Not Adopting

| Pattern | Reason |
|---------|--------|
| File system tools | Claude has native file access |
| Project-scoped security | Not needed - MCP provides image access, not file access |
| MCP Resources | Redundant with Claude's file reading |
| HTTP transport | STDIO sufficient for Claude Code integration |
