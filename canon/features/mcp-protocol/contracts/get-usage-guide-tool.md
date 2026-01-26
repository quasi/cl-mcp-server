---
type: contract
name: get-usage-guide-tool
version: 0.1.0
---

# Get-Usage-Guide Tool Contract

Returns comprehensive documentation on how to use the MCP server effectively. Designed to be called at the start of a session to learn best practices.

## Tool Definition

```json
{
  "name": "get-usage-guide",
  "description": "Get the recommended workflow guide for using this Lisp MCP server effectively. RECOMMENDED: Call this when starting a new session to learn best practices for incremental development, syntax validation, and effective tool usage.",
  "inputSchema": {
    "type": "object",
    "properties": {}
  }
}
```

## Output Format

Returns a markdown-formatted guide containing:

1. **Quick Start** - Overview of server capabilities
2. **Available Tools** - Table of all tools with purposes
3. **Recommended Workflow** - Step-by-step best practices
4. **Common Patterns** - Code examples for typical use cases
5. **Anti-Patterns** - What to avoid
6. **Error Recovery** - How to recover from common issues
7. **Session Persistence** - Understanding session state

## Purpose

This tool serves as onboarding documentation for AI agents using the MCP server. By calling this tool first, agents learn:

- Which tools to use for which purposes
- The validate-before-save workflow
- Incremental development patterns
- How to avoid common mistakes

## Usage Pattern

```
Agent starts session with Lisp MCP server
↓
Agent calls get-usage-guide (no arguments)
↓
Agent reads guide and understands best practices
↓
Agent follows recommended workflows during development
```

## Tool Description Design

The tool description includes "RECOMMENDED: Call this when starting a new session" to encourage agents to invoke it proactively. This is intentional - the description is visible in the `tools/list` response and serves as a hint.

## Relationship to Other Tools

This tool is informational only. It does not:
- Execute any code
- Modify session state
- Require any arguments

It complements other tools by explaining when and how to use them effectively.
