---
feature: asdf-integration
verification_status: specified
last_updated: 2026-01-26
---

# ASDF Integration Verification

Test specifications for the asdf-integration feature.

## Purpose

Verification tests ensure ASDF and Quicklisp integration tools correctly load systems,
track dependencies, and maintain system state safely.

## Test Organization

```
verification/
├── contracts/           # Contract conformance tests
│   └── quickload-tool.test.md
├── properties/          # Property-based tests
│   ├── system-dependency-accuracy.test.md
│   ├── quickload-safety.test.md
│   ├── system-state-tracking.test.md
│   ├── load-idempotency.test.md
│   └── dependency-resolution-correctness.test.md
└── scenarios/           # Scenario-based tests (optional)
```

## Running Tests

From the MCP server test suite:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:asdf-integration-suite)"
```

## Test Requirements

### Contract Tests

- Systems load successfully
- Dependencies resolved correctly
- System information accurate
- Quicklisp integration works
- Error handling for missing systems

### Property Tests

- Dependency graphs accurate
- Load operations idempotent
- System state tracked correctly
- Quickload is safe (no side effects)
- Dependency resolution complete

## Coverage Goals

- Standard systems (alexandria, etc.)
- Systems with dependencies
- Missing systems (error cases)
- Already-loaded systems
- Circular dependency detection
