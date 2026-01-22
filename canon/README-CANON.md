# CL-MCP-Server Canon - Agent Reference

This document provides guidance for AI agents working with this Canon specification.

## Canon Infrastructure Version

0.1.0

## Quick Reference

### Reading This Canon

1. Start with `canon.yaml` for system overview and feature list
2. Read `core/foundation/vocabulary.md` for domain terminology
3. Check `core/foundation/invariants.md` for inviolable rules
4. Explore features in `features/{name}/` directories

### Feature Structure

Each feature contains:
- `feature.yaml` - Metadata, dependencies, status
- `vocabulary.md` - Feature-specific terms
- `contracts/` - Interface specifications (JSON Schema, etc.)
- `properties/` - Invariants and rules
- `scenarios/` - Concrete behavioral examples
- `verification/` - Test specifications
- `decisions/` - Design decisions (ADR format)
- `.context.yaml` - Context manifest for efficient navigation (see below)

### Context Manifests

Each feature has a `.context.yaml` file that provides efficient navigation for agents working with the Canon.

**Purpose**: Context manifests help agents load exactly what they need without reading the entire Canon, optimizing for token usage and comprehension.

**Location**: `features/{feature-name}/.context.yaml`

**What They Contain**:
- `essential` - Minimum files to understand the feature
- `per_contract` - Targeted paths for specific contracts
- `reference` - Additional materials (invariants, dependencies)
- `token_estimate` - Approximate token count for the feature
- `summary` - Quick feature overview

**Usage Example**:

```yaml
# To understand mcp-protocol feature:
essential:
  - core/foundation/vocabulary.md  # Read these first
  - feature.yaml
  - contracts/initialization.md

# To work on initialization contract specifically:
per_contract:
  initialization:
    - contracts/initialization.md
    - scenarios/initialization-handshake.md
```

**When to Use Context Manifests**:
- Exploring a new feature for the first time
- Working on a specific contract within a feature
- Planning token budget for feature understanding
- Finding cross-feature dependencies quickly

### Status Meanings

- **draft**: Initial specification, subject to change
- **review**: Ready for feedback
- **stable**: Approved, changes require evolution process
- **deprecated**: Being phased out

### Working With This Canon

**To implement**: Read contracts and scenarios, implement to satisfy them.

**To verify**: Run scenarios against implementation, check properties hold.

**To extend**: Add new features or evolve existing ones following the Canon process.

## System Overview

CL-MCP-Server is an MCP server enabling Claude to evaluate Common Lisp code.

### Core Invariants (Summary)

1. Every valid request gets exactly one response
2. Server never crashes on evaluation errors
3. Session state persists across evaluations
4. Output streams (values, stdout, stderr) are distinguishable
5. All messages conform to JSON-RPC 2.0
6. Condition types are preserved in error reports

### Key Contracts

- `core/contracts/shared-types.md` - JSON-RPC and MCP type definitions
- Feature-specific contracts define interfaces per feature

## Implementation Notes

Target: Common Lisp (SBCL recommended)

Key considerations:
- Use `handler-case`/`handler-bind` for condition capture
- Redirect `*standard-output*` and `*error-output*` during evaluation
- Consider `sb-ext:restrict-compiler-policy` for sandboxing
- Use `yason` or `cl-json` for JSON handling
