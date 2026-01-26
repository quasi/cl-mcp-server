---
feature: clos-intelligence
verification_status: specified
last_updated: 2026-01-26
---

# CLOS Intelligence Verification

Test specifications for the clos-intelligence feature.

## Purpose

Verification tests ensure the CLOS intelligence tools (class-info, find-methods)
correctly introspect class hierarchies, slots, and method specializations.

## Test Organization

```
verification/
├── contracts/           # Contract conformance tests
│   └── class-info-tool.test.md
├── properties/          # Property-based tests
│   ├── class-hierarchy-accuracy.test.md
│   ├── slot-information-completeness.test.md
│   ├── method-discovery-completeness.test.md
│   └── mop-reflection-safety.test.md
└── scenarios/           # Scenario-based tests (optional)
```

## Running Tests

From the MCP server test suite:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:clos-intelligence-suite)"
```

## Test Requirements

### Contract Tests

- Class hierarchy correctly reported
- Slot information complete
- Method specializations accurate
- Metaclass information present
- Standard classes and custom classes

### Property Tests

- Superclass relationships transitive
- Slot inheritance correct
- Method discovery complete
- MOP operations safe
- Reflection doesn't modify state

## Coverage Goals

- Standard classes (list, array, etc.)
- User-defined classes
- Multiple inheritance
- Slot options (readers, writers, initforms)
- Method combinations
