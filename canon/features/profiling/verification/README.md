---
feature: profiling
verification_status: specified
last_updated: 2026-01-26
---

# Profiling Verification

Test specifications for the profiling feature.

## Purpose

Verification tests ensure profiling tools accurately identify hot spots, measure overhead,
and track memory allocation without corrupting execution.

## Test Organization

```
verification/
├── contracts/           # Contract conformance tests
│   └── profile-code-tool.test.md
├── properties/          # Property-based tests
│   ├── sampling-accuracy.test.md
│   ├── profiling-overhead-bounded.test.md
│   ├── hot-spot-identification.test.md
│   ├── memory-tracking-accuracy.test.md
│   └── profiling-safety.test.md
└── scenarios/           # Scenario-based tests (optional)
```

## Running Tests

From the MCP server test suite:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:profiling-suite)"
```

## Test Requirements

### Contract Tests

- Profiling captures execution data
- Hot spots identified correctly
- Sample counts accurate
- Overhead reported
- Results formatted properly

### Property Tests

- Sampling accuracy within bounds
- Overhead proportional to samples
- Hot spots match actual execution
- Memory tracking accurate
- Profiling doesn't crash code

## Coverage Goals

- CPU-bound profiling
- Memory-allocation profiling
- Time-based profiling
- Nested function calls
- Recursive functions
