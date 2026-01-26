# Introspection Feature

Phase A: Core Introspection Tools for SBCL image inspection.

## Overview

Provides deep inspection of the running SBCL image, allowing Claude to "see"
the Lisp environment as clearly as a human using Sly/SLIME. Uses sb-introspect
for SBCL-specific capabilities including source locations, arglists, and
cross-reference databases.

## Status

- **Version**: 0.1.0
- **Status**: Draft
- **SBCL-Specific**: Yes (requires sb-introspect)

## Contracts

| Contract | Tool | Description |
|----------|------|-------------|
| [describe-symbol-tool](contracts/describe-symbol-tool.md) | describe-symbol | Comprehensive symbol information |
| [apropos-search-tool](contracts/apropos-search-tool.md) | apropos-search | Symbol discovery with filtering |
| [who-calls-tool](contracts/who-calls-tool.md) | who-calls | Find function callers |
| [who-references-tool](contracts/who-references-tool.md) | who-references | Find variable readers |
| [macroexpand-form-tool](contracts/macroexpand-form-tool.md) | macroexpand-form | Macro expansion |
| [validate-syntax-tool](contracts/validate-syntax-tool.md) | validate-syntax | Syntax validation without evaluation |

## Scenarios

| Scenario | Description |
|----------|-------------|
| [symbol-inspection](scenarios/symbol-inspection.md) | Querying detailed symbol information |
| [symbol-discovery](scenarios/symbol-discovery.md) | Finding symbols by pattern |
| [cross-reference](scenarios/cross-reference.md) | Navigating code relationships |
| [macro-understanding](scenarios/macro-understanding.md) | Understanding macro transformations |
| [syntax-validation](scenarios/syntax-validation.md) | Validating code syntax before saving |

## Vocabulary

See [vocabulary.md](vocabulary.md) for terms specific to introspection:
- Symbol Type
- Symbol Information
- Apropos
- Cross-Reference (XREF)
- sb-introspect
- Macro Expansion
- Definition Source

## Dependencies

- **core**: Session management for package context

## Key Capabilities

### Symbol Information
Query comprehensive details about any symbol:
- Type classification (function, macro, variable, class, generic-function)
- Arglists for callable symbols
- Documentation strings
- Current values for bound variables
- Source file locations

### Symbol Discovery
Find symbols matching patterns:
- Case-insensitive substring search
- Package scoping
- Type filtering

### Cross-Reference Navigation
Navigate code relationships:
- Find all callers of a function
- Find all code referencing a variable
- Populated by SBCL during compilation

### Macro Understanding
Inspect macro transformations:
- Single-step expansion (macroexpand-1)
- Full recursive expansion (macroexpand)
- Understand what macros generate

## Implementation Notes

All tools are read-only and don't modify the Lisp image state.
Cross-reference data depends on compilation with debug info enabled.
