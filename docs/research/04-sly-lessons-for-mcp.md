# Lessons from Sly/SLYNK for cl-mcp-server

## Executive Summary

Sly (Superior Lisp Interaction Mode for Emacs) with SLYNK backend has been the gold standard for human Common Lisp development for decades. By studying Sly's architecture and the agent-q project (which builds AI capabilities on top of Sly), we can extract key lessons for making cl-mcp-server the best companion for Claude to do Common Lisp programming.

**Core Insight**: The power of Lisp development comes not from text editing, but from **live interaction with a running image**. Claude already has excellent text manipulation. What Claude lacks—and what cl-mcp-server can provide—is the ability to **ask the Lisp image what's true**.

---

## What Makes Sly Powerful

### 1. The REPL as Truth Source

Sly's fundamental insight: The running Lisp image is the source of truth.

```
Traditional IDE: Parse code → Build AST → Guess meaning
Sly Approach:    Ask image → Get actual state → Know truth
```

**For cl-mcp-server**: Claude shouldn't guess what a function does—it should ask the image via introspection tools.

### 2. Sly's Tool Categories

| Category | Sly Feature | Purpose |
|----------|-------------|---------|
| **Introspection** | `describe`, `apropos`, `who-calls` | Understand existing code |
| **Evaluation** | REPL, `C-x C-e`, compile | Run and test code |
| **Navigation** | `M-.`, xref | Find definitions and usages |
| **Debugging** | Debugger, restarts, inspector | Fix problems |
| **Completion** | Symbol completion, arglist | Write code correctly |

**For cl-mcp-server**: Map these capabilities to MCP tools.

### 3. The Closed Loop

From agent-q research, the key superpower:

```
LLM generates code
    → Agent evaluates in REPL
    → Agent observes result
    → Agent iterates
```

Unlike other languages where AI assistants can only suggest code and hope it works, Lisp allows:
1. Propose a solution
2. Actually run it
3. See what happened
4. Fix issues and try again

**For cl-mcp-server**: This is already our strength with `evaluate-lisp`. Enhance it.

---

## Key Sly/SLYNK Capabilities to Mirror

### 1. Introspection Tools (High Priority)

From agent-q's `introspection.lisp`:

| Sly Capability | MCP Tool Name | Description |
|----------------|---------------|-------------|
| `describe` | `describe-symbol` | Full symbol info (type, value, docstring, location) |
| `apropos` | `apropos-search` | Find symbols matching pattern |
| `arglist` | `function-arglist` | Get function parameters |
| `who-calls` | `who-calls` | Find all callers (xref) |
| `who-references` | `who-references` | Find all references (xref) |
| `class-slots` | `class-slots` | CLOS slot definitions |
| `class-hierarchy` | `class-hierarchy` | Superclass/subclass tree |
| `macroexpand` | `macroexpand-form` | Expand macros |
| `list-package` | `list-package-symbols` | Package contents |

**SBCL-Specific Features** (available when running on SBCL):

```lisp
;; These require sb-introspect package
(sb-introspect:function-lambda-list #'my-function)
(sb-introspect:who-calls 'my-function)
(sb-introspect:who-references 'my-symbol)
(sb-introspect:find-definition-source #'my-function)
```

### 2. Evaluation Enhancement (Medium Priority)

Current `evaluate-lisp` is good. Enhance with:

| Enhancement | Value |
|-------------|-------|
| Package context | Evaluate in correct package |
| Multiple values | Return all values, not just first |
| Compilation mode | `compile nil` for testing compiled behavior |
| Time profiling | `time` wrapper for performance insight |
| Trace output | Capture trace during evaluation |

### 3. Error/Condition Intelligence (High Priority)

Sly's debugger is powerful because it preserves context:

```lisp
;; When error occurs, full stack is available
;; Restarts allow recovery without unwinding

(restart-case
    (risky-operation)
  (use-cached-value () (get-from-cache))
  (retry () (risky-operation))
  (skip () nil))
```

**For cl-mcp-server**:
- `list-restarts` - Show available restarts when error occurs
- `describe-condition` - Detailed error information
- `get-backtrace` - Stack trace with locals
- `invoke-restart` - Programmatically recover (with approval)

### 4. Definition Tracking (Already Strong)

cl-mcp-server already has `list-definitions` which is unique. Enhance with:

| Enhancement | Value |
|-------------|-------|
| Source location | Track where definitions came from |
| Dependency graph | Which definitions depend on which |
| History | When was each definition created/modified |
| Diff from baseline | What changed since session started |

---

## Architecture Lessons from Agent-Q

### 1. Tool Safety Levels

Agent-q implements three safety levels:

```lisp
:safe      ; Read-only: describe, apropos, who-calls
:moderate  ; State-changing but reversible: eval, compile
:dangerous ; Potentially destructive: delete-file, invoke-restart
```

**For cl-mcp-server**:
- Group tools by safety
- Default to safe operations
- Require explicit opt-in for dangerous operations
- Document trust model clearly

### 2. Tool Registry Pattern

Agent-q uses a structured registry:

```lisp
(define-tool "describe_symbol"
  "Get detailed information about a Lisp symbol..."
  '((:name "symbol" :type :string :description "...")
    (:name "package" :type :string :description "..."))
  :required '("symbol")
  :safety-level :safe
  :categories '(:introspection)
  :handler (lambda (args) ...))
```

**For cl-mcp-server**: Consider adopting a similar `deftool` macro for cleaner definitions.

### 3. Hybrid Architecture

Agent-q separates concerns:

```
Emacs (UI) ←──SLY RPC──→ Lisp (Logic/Persistence)
```

**For cl-mcp-server**:
```
Claude (Planning) ←──MCP──→ SBCL (Execution/Truth)
```

Key insight: Keep the Lisp side as the source of truth. MCP is just the communication channel.

---

## Recommended Tool Additions for cl-mcp-server

### Phase 1: Core Introspection (Critical)

These tools let Claude "see" the codebase:

```
1. describe-symbol    - Full symbol info
2. apropos-search     - Find symbols by pattern
3. function-arglist   - Get parameter list
4. macroexpand-form   - Understand macros
5. package-info       - List package contents
```

### Phase 2: Cross-Reference (High Value)

These tools let Claude understand code relationships:

```
6. who-calls          - Find all callers
7. who-references     - Find all references
8. class-slots        - CLOS class structure
9. class-hierarchy    - Inheritance tree
10. method-specializers - Find methods for class
```

### Phase 3: Enhanced Evaluation (Medium Value)

Improve the evaluation experience:

```
11. compile-form      - Compile (not just eval)
12. disassemble-function - See generated code
13. time-execution    - Performance profiling
14. trace-function    - Enable tracing
15. untrace-function  - Disable tracing
```

### Phase 4: Error Intelligence (Medium Value)

Better error handling:

```
16. describe-condition - Detailed error info
17. get-backtrace      - Stack trace
18. list-restarts      - Available recovery options
19. inspect-object     - Object structure
```

---

## What NOT to Copy from Sly

### 1. Interactive Debugging

Sly's interactive debugger assumes a human at the keyboard. For Claude:
- Don't try to replicate the full debugger experience
- Instead: Capture error information as structured data
- Let Claude analyze and propose fixes

### 2. Real-time Completion

Sly's completion works character-by-character. For Claude:
- Not needed - Claude can call `apropos` explicitly
- Focus on comprehensive symbol search instead

### 3. Editor Integration

Sly deeply integrates with Emacs buffer state. For Claude:
- Claude has its own text manipulation capabilities
- Focus on providing information, not editing files

---

## Implementation Priority Matrix

| Tool | Complexity | Value | Priority | Notes |
|------|------------|-------|----------|-------|
| describe-symbol | Low | High | P0 | Foundation for understanding |
| apropos-search | Low | High | P0 | Discovery mechanism |
| function-arglist | Low | High | P0 | API understanding |
| who-calls | Medium | High | P1 | Impact analysis (needs xref) |
| who-references | Medium | High | P1 | Impact analysis (needs xref) |
| class-slots | Medium | Medium | P2 | CLOS understanding |
| class-hierarchy | Medium | Medium | P2 | CLOS understanding |
| macroexpand-form | Low | Medium | P1 | Macro understanding |
| compile-form | Low | Medium | P2 | Testing compiled behavior |
| describe-condition | Medium | High | P1 | Better error handling |
| get-backtrace | Medium | High | P1 | Debugging |
| disassemble | Low | Low | P3 | Optimization only |
| time-execution | Low | Medium | P2 | Performance work |

---

## Design Decisions for cl-mcp-server

### D1: SBCL-Only vs. Portable

**Recommendation**: SBCL-only for now.

**Rationale**:
- `sb-introspect` provides rich capabilities not available portably
- SBCL is the de facto standard for production CL
- Agent-q made the same choice
- Can add portable fallbacks later if needed

### D2: Tool Granularity

**Recommendation**: Many focused tools, not few monolithic tools.

**Rationale**:
- LLMs work better with specific tools for specific purposes
- Easier to document and maintain
- Follows Sly's pattern (many small commands)

Example: Instead of one `introspect` tool, have:
- `describe-symbol`
- `function-arglist`
- `who-calls`
- etc.

### D3: Output Format

**Recommendation**: Structured data with readable text fallback.

```lisp
;; Return plist that can be formatted as text
(:symbol "FOO"
 :type :function
 :package "MY-PACKAGE"
 :arglist (x y &optional z)
 :documentation "Computes foo of x and y"
 :source-file "/path/to/file.lisp"
 :source-line 42)
```

**Rationale**:
- Structured data enables Claude to reason about results
- Text representation ensures human readability
- Follows MCP conventions

### D4: Error Representation

**Recommendation**: Rich structured errors, not just strings.

```lisp
(:error-type "TYPE-ERROR"
 :message "The value 42 is not of type STRING"
 :expected-type "STRING"
 :datum 42
 :backtrace (...)
 :restarts ((:name "USE-VALUE" :description "Use a different value")
            (:name "ABORT" :description "Abort evaluation")))
```

**Rationale**:
- Claude can make intelligent recovery decisions
- Mirrors Sly's condition system intelligence

---

## The Ultimate Goal

Make Claude as effective at Lisp development as a human with Sly.

| Human + Sly | Claude + cl-mcp-server |
|-------------|------------------------|
| `C-c C-d d` (describe) | `describe-symbol` tool |
| `M-.` (go to definition) | `find-definition` tool |
| `C-c C-w c` (who-calls) | `who-calls` tool |
| Debugger buffer | `get-backtrace` + `list-restarts` |
| REPL evaluation | `evaluate-lisp` tool |
| Definition tracking | `list-definitions` tool (already have!) |

The combination of Claude's reasoning + cl-mcp-server's image access should equal or exceed what a human can do with Sly.

---

## Key Takeaways

1. **The image is truth** - Let Claude ask the image, not guess from text
2. **Introspection before evaluation** - Claude should understand before modifying
3. **Structured errors** - Rich error data enables intelligent recovery
4. **SBCL is fine** - Don't sacrifice capability for theoretical portability
5. **Many small tools** - Better for LLM tool use patterns
6. **Our unique strength** - Definition tracking sets us apart; enhance it

---

## References

- Agent-Q Research: `/Users/quasi/quasilabs/projects/agent-q/docs/research/`
- Sly Documentation: https://github.com/joaotavora/sly
- SLYNK Protocol: Sly's `slynk/` directory
- sb-introspect: SBCL manual, Chapter 13
- cl-mcp-server existing research: `docs/research/01-03`
