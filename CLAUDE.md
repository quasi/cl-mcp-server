# CLAUDE.md

Instructions for Claude Code (claude.ai/code) when working with this repository.

**See [AGENT.md](AGENT.md) for full contributing guidelines**, including:
- Build and test commands
- Code conventions and naming
- Architecture rules and invariants
- Protocol conformance requirements
- Testing strategy

## Quick Reference

```bash
# Load and run server
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server)" \
     --eval "(cl-mcp-server:start)"

# Run tests
sbcl --load cl-mcp-server.asd \
     --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(asdf:test-system :cl-mcp-server)"
```

## Issue Tracking

This project uses **bd (beads)** for issue tracking.

- `bd ready` - Find unblocked work
- `bd create "Title" --type task --priority 2` - Create issue
- `bd close <id>` - Complete work
- `bd sync --flush-only` - Export to JSONL

