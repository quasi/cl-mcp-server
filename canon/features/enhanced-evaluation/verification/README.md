# Enhanced Evaluation Verification

Test specifications for the enhanced-evaluation feature.

## Purpose

Verification tests ensure the enhanced-evaluation tools (compile-form, time-execution)
conform to their contracts and satisfy the stated properties.

## Test Organization

```
verification/
├── contracts/           # Contract conformance tests
│   ├── compile-form-tool.test.md
│   └── time-execution-tool.test.md
├── properties/          # Property-based tests
│   ├── compilation-safety.test.md
│   └── timing-accuracy.test.md
└── scenarios/           # Scenario-based tests
    ├── compilation-checking.test.md
    └── performance-profiling.test.md
```

## Running Tests

From the MCP server test suite:

```lisp
(asdf:test-system :cl-mcp-server)
```

Specific feature tests:

```lisp
(asdf:test-system :cl-mcp-server/tests/enhanced-evaluation)
```

## Test Requirements

### Contract Tests

Each tool contract has a corresponding test file verifying:
- Input schema validation
- Output format compliance
- Error handling behavior
- Edge case handling

### Property Tests

Each property has tests verifying:
- The property holds across random inputs
- Counterexamples cause expected failures
- Shrinking identifies minimal failures

### Scenario Tests

Each scenario has tests showing:
- End-to-end workflow
- Tool usage patterns
- Expected outcomes

## Test Data

Test fixtures are located in:
- `tests/fixtures/enhanced-evaluation/`

## Coverage Goals

- All contract preconditions tested
- All property assertions verified
- All scenario steps executable
- Edge cases and error paths covered
