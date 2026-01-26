# Recommendations: Future Direction for cl-mcp-server

## Strategic Position

Based on our comparative research, cl-mcp-server occupies a unique niche:

```
                    Feature-Rich
                         │
                         │ cl-ai-project/cl-mcp
                         │
    Complex ─────────────┼───────────────── Simple
                         │
                         │     ● cl-mcp-server
    belyak/mcp-srv-lisp  │
                         │
                    Evaluation-Focused
```

**Our differentiation**: The only pure-CL implementation focused on interactive code evaluation with session persistence and definition tracking.

---

## Recommended Direction

### Option A: Agent-Optimized REPL Server (Recommended)

**Vision**: Become the best MCP server for AI-assisted Lisp development.

**Focus Areas**:
1. Enhance code evaluation (already strong)
2. Add security model (critical gap)
3. Add file tools (high value)
4. Add search tools (high value)

**Avoid**: Full MCP feature coverage (resources, prompts) unless needed.

**Rationale**: Claude already has file access and search capabilities. Our value is the **live Lisp image** that Claude cannot replicate.

### Option B: Complete MCP Framework

**Vision**: Provide the most compliant CL MCP implementation.

**Focus Areas**:
1. Add resources support
2. Add prompts support
3. Add HTTP transport
4. Comprehensive MCP compliance testing

**Rationale**: Compete with belyak/mcp-srv-lisp and 40ants/mcp on breadth.

### Option C: Minimal Evaluation Server

**Vision**: Stay lean, do one thing well.

**Focus Areas**:
1. Polish existing code evaluation
2. Add security/sandboxing
3. Excellent documentation
4. Easy installation

**Rationale**: Minimize maintenance burden, maximize reliability.

---

## Cost-Benefit Analysis: Key Features

### 1. Security Model (Project-Scoping)

**Cost**: Medium
- ~500 LOC for path validation
- Integration with file tools
- Testing across edge cases

**Benefit**: High
- Enables safer deployment
- Required for enterprise adoption
- Protects against accidental damage

**Recommendation**: **ADOPT** - Critical for production use

### 2. File System Tools

**Cost**: Medium
- ~800 LOC for list/read/write
- Path security integration
- Error handling for edge cases

**Benefit**: High
- Enables full development workflow
- Claude can read project context
- Reduces context-switching

**Recommendation**: **ADOPT** - High value, manageable cost

### 3. Code Search Tools

**Cost**: Medium-High
- ~600 LOC for basic search
- Integration with ASDF for system awareness
- May need cl-ppcre dependency

**Benefit**: Medium
- Claude has grep/search already
- Lisp-aware search is differentiating
- Signature return is valuable

**Recommendation**: **CONSIDER** - Nice to have, not critical

### 4. HTTP Transport

**Cost**: High
- New transport abstraction layer
- Hunchentoot/Clack dependency
- SSE for streaming (optional)
- Security considerations (CORS, auth)

**Benefit**: Medium
- Enables cloud deployment
- Multi-client scenarios
- Remote development

**Recommendation**: **DEFER** - Add when needed, stdio works for local use

### 5. Resources Support

**Cost**: Medium
- ~400 LOC for resource registry
- URI template handling
- MIME type detection

**Benefit**: Low
- Claude can read files directly
- Overlaps with file tools
- Limited unique value

**Recommendation**: **SKIP** - Low value for our use case

### 6. Prompts Support

**Cost**: Low
- ~300 LOC for prompt registry
- Template interpolation

**Benefit**: Low
- Agent guidance can be in AGENT.md
- MCP prompts less flexible than markdown
- Limited adoption in practice

**Recommendation**: **SKIP** - Document-based guidance is better

### 7. Structure-Aware Editing (Eclector)

**Cost**: High
- Eclector dependency
- CST manipulation complexity
- Careful testing required

**Benefit**: Medium
- Surgical code changes
- Preserves formatting
- Professional-grade editing

**Recommendation**: **DEFER** - Interesting but not essential

### 8. SBCL Introspection Tools

**Cost**: Medium
- sb-introspect integration
- Function/variable metadata
- Source location extraction

**Benefit**: Medium
- `describe`/`inspect` via MCP
- Documentation lookup
- Powerful debugging

**Recommendation**: **CONSIDER** - Valuable for debugging workflows

---

## Design Decisions to Make

### D1: Security Approach

**Options**:

A. **Project-Scoped Access** (like cl-ai-project)
   - Pros: Pure Lisp, no external deps
   - Cons: Still allows code execution within scope

B. **Sandboxed Evaluation** (restricted reader/evaluator)
   - Pros: Limits dangerous operations
   - Cons: Complex, may break valid code

C. **Trust Model** (current approach)
   - Pros: Maximum flexibility
   - Cons: Dangerous in shared environments

**Recommendation**: **Option A** - Project-scoping for files, trust for code evaluation. Document the trust model clearly.

### D2: Tool Registration Pattern

**Options**:

A. **Current** (hash-table + register-tool)
   - Pros: Simple, works
   - Cons: Verbose for new tools

B. **Macro-Based** (like 40ants define-tool)
   - Pros: Declarative, less boilerplate
   - Cons: Macro complexity

C. **Template-Based** (like belyak)
   - Pros: Data-driven configuration
   - Cons: Runtime parsing, less type safety

**Recommendation**: **Option B** - Add a `deftool` macro for cleaner definitions while keeping `register-tool` for programmatic use.

### D3: Error Reporting Depth

**Options**:

A. **Minimal** - Error message only
B. **Standard** - Message + condition type (current)
C. **Rich** - Message + type + backtrace + locals

**Recommendation**: **Option C** for evaluation errors - backtraces are invaluable for debugging. Already partially implemented.

### D4: Multi-Implementation Support

**Options**:

A. **SBCL-Only** - Optimize for one implementation
B. **Portable Subset** - Use only portable features
C. **Conditional Features** - Core portable, extras per-impl

**Recommendation**: **Option C** - Core evaluation portable, SBCL gets extras (introspection, better backtraces).

---

## Implementation Roadmap

### Phase 1: Security & Files (High Priority)

1. **Project Root Management**
   - `set-project-root` tool
   - `*project-root*` dynamic variable
   - Path validation functions

2. **File Tools**
   - `list-directory` (filtered)
   - `read-file` (validated path)
   - `write-file` (within project)

3. **Audit Logging**
   - File operations logged
   - Configurable log level

### Phase 2: Developer Experience (Medium Priority)

4. **Tool Definition Macro**
   ```lisp
   (deftool list-directory
     "List files in directory"
     (:param path :string :required t)
     (:param include-hidden :boolean)
     ...)
   ```

5. **Package Management**
   - `create-package` tool
   - `in-package` support in evaluation

6. **Enhanced Introspection**
   - `describe-symbol` tool
   - `find-definition` tool

### Phase 3: Optional Enhancements (Low Priority)

7. **HTTP Transport** (if needed)
8. **Search Tools** (if needed)
9. **Structure-Aware Editing** (if needed)

---

## What NOT To Do

Based on research findings, avoid these paths:

### Don't Adopt

1. **Template-Based Tool Definitions** (belyak)
   - Overly complex for our needs
   - JSON templates are awkward in Lisp

2. **HTTP-First Design** (lisply-mcp)
   - Adds latency and complexity
   - STDIO is simpler and faster for local use

3. **40ants Ecosystem Dependency**
   - Lock-in to their stack
   - Requires patches for strict MCP compliance

4. **Docker Containerization** (lisply-mcp)
   - Overhead for local development
   - Complicates debugging

### Don't Compete On

1. **Full MCP Compliance** - Resources and prompts have limited value
2. **Tool Count** - Quality over quantity
3. **Multi-Language Support** - Focus on CL excellence

---

## Success Metrics

How will we know if our direction is correct?

### Short-Term (3 months)

- [ ] Security model implemented and tested
- [ ] File tools working with path validation
- [ ] Users can safely expose cl-mcp-server to Claude

### Medium-Term (6 months)

- [ ] Feedback from users guides feature prioritization
- [ ] Tool definition macro reduces boilerplate
- [ ] Documentation enables self-service adoption

### Long-Term (12 months)

- [ ] cl-mcp-server becomes the recommended CL MCP server
- [ ] Community contributions expand tool library
- [ ] Integration with popular CL editors (Emacs, VSCode)

---

## Conclusion

The research reveals that **cl-mcp-server is well-positioned** in the CL MCP ecosystem:

- **Unique strength**: Definition tracking and session persistence
- **Key gap**: Security model for file operations
- **Recommended focus**: Agent-optimized REPL server, not full MCP framework

The path forward is clear:
1. Add project-scoped security
2. Add file tools
3. Improve tool definition ergonomics
4. Document trust model explicitly

This positions cl-mcp-server as the premier tool for AI-assisted Common Lisp development.
