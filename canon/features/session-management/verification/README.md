---
feature: session-management
verification_status: specification-complete
last_updated: 2026-01-26
---

# Session Management Verification

Test suite for the session-management feature, covering contracts, scenarios, and properties.

## Test Organization

```
verification/
├── contracts/          # Individual contract tests
│   └── session-state.test.md
├── scenarios/          # Full scenario workflows
│   ├── session-reset.test.md
│   └── state-persistence.test.md
├── properties/         # Property-based tests
│   ├── state-persistence.test.md
│   └── session-lifecycle.test.md
└── README.md          # This file
```

## Running Tests

### Quick Smoke Test

Run basic session state tests:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-smoke-suite)"
```

Duration: ~15 seconds

### Contract Tests

Test each contract in isolation:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-contract-suite)"
```

Duration: ~30 seconds

### Scenario Tests

Test complete workflows:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-scenario-suite)"
```

Duration: ~45 seconds

### Property Tests

Run property-based tests with default trials (100):

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-property-suite)"
```

Duration: ~2 minutes

### Extended Property Tests

Run with 1000 trials for thorough verification:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(setf cl-quickcheck:*num-trials* 1000)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-property-suite)"
```

Duration: ~20 minutes (CI only)

### Full Suite

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:session-management-suite)"
```

Duration: ~3 minutes

## Coverage

### Contracts Covered

| Contract | Test File | Status |
|----------|-----------|--------|
| session-state | contracts/session-state.test.md | ✅ |

### Scenarios Covered

| Scenario | Test File | Status |
|----------|-----------|--------|
| session-reset | scenarios/session-reset.test.md | ✅ |
| state-persistence | scenarios/state-persistence.test.md | ✅ |

### Properties Covered

| Property | Test File | Status |
|----------|-----------|--------|
| state-persistence | properties/state-persistence.test.md | ✅ |
| session-lifecycle | properties/session-lifecycle.test.md | ✅ |
| package-context | ❌ Not yet implemented | ⏳ |
| definition-visibility | ❌ Not yet implemented | ⏳ |

## Test Implementation Status

Current implementation: **Specifications Complete, Code Pending**

These test files are **specifications** written in markdown with embedded Lisp test code. They document:
- What to test
- How to test it
- Expected results

**Next step**: Implement actual test files in `tests/` directory:
```
tests/
├── session-management/
│   ├── contracts-test.lisp
│   ├── scenarios-test.lisp
│   └── properties-test.lisp
```

## Test Helpers Required

The following test helper functions need to be implemented:

### Server Management
- `make-initialized-test-server` - Create and initialize a test server instance
- `stop-test-server` - Clean up test server
- `send-request` - Send JSON-RPC request to server

### Response Inspection
- `result-response-p` - Check if response has result field
- `error-response-p` - Check if response has error field
- `is-error` - Check if result has isError=true
- `extract-result-text` - Extract text content from result

### Generators (for property tests)
- `random-choice` - Pick random element from list
- `generate-random-definition` - Generate valid definition forms

## Notes

- All test specifications use FiveAM test framework
- Property tests use randomized generation for edge case discovery
- Tests assume isolated server instances per test
- Tests verify both successful operations and error conditions
- Session state must persist between tool calls
- Reset must completely clear user-defined state
