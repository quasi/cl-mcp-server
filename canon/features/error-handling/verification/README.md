---
feature: error-handling
verification_status: specification-complete
last_updated: 2026-01-26
---

# Error Handling Verification

Test suite for the error-handling feature, covering contracts, scenarios, and properties.

## Test Organization

```
verification/
├── contracts/          # Individual contract tests
│   └── condition-report.test.md
├── scenarios/          # Full scenario workflows
│   └── evaluation-errors.test.md
├── properties/         # Property-based tests
│   ├── error-capture-stability.test.md
│   └── structured-error-response.test.md
└── README.md          # This file
```

## Running Tests

### Quick Smoke Test

Run basic error handling tests:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-smoke-suite)"
```

Duration: ~10 seconds

### Contract Tests

Test each contract in isolation:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-contract-suite)"
```

Duration: ~30 seconds

### Scenario Tests

Test complete error handling workflows:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-scenario-suite)"
```

Duration: ~45 seconds

### Property Tests

Run property-based tests with default trials (100):

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-property-suite)"
```

Duration: ~3 minutes

### Stress Test

Run with 1000 trials for stress testing:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(setf cl-quickcheck:*num-trials* 1000)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-property-suite)"
```

Duration: ~30 minutes (CI only)

### Full Suite

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-handling-suite)"
```

Duration: ~4 minutes

## Coverage

### Contracts Covered

| Contract | Test File | Status |
|----------|-----------|--------|
| condition-report | contracts/condition-report.test.md | ✅ |

### Scenarios Covered

| Scenario | Test File | Status |
|----------|-----------|--------|
| evaluation-errors | scenarios/evaluation-errors.test.md | ✅ |

### Properties Covered

| Property | Test File | Status |
|----------|-----------|--------|
| error-capture-stability | properties/error-capture-stability.test.md | ✅ |
| structured-error-response | properties/structured-error-response.test.md | ✅ |
| condition-type-preservation | ❌ Not yet implemented | ⏳ |
| warning-capture-muffling | ❌ Not yet implemented | ⏳ |

## Test Implementation Status

Current implementation: **Specifications Complete, Code Pending**

These test files are **specifications** written in markdown with embedded Lisp test code. They document:
- What to test
- How to test it
- Expected results

**Next step**: Implement actual test files in `tests/` directory:
```
tests/
├── error-handling/
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
- `is-error` - Check if result has isError=true
- `extract-result-text` - Extract text content from result

### Generators (for property tests)
- `generate-error-code` - Generate code that produces various errors
- `random-choice` - Pick random element from list

## Error Types Covered

The test suite covers all common Lisp condition types:

### Evaluation Errors
- `UNDEFINED-FUNCTION` - Unknown function calls
- `UNBOUND-VARIABLE` - Unknown variable references
- `TYPE-ERROR` - Type mismatches
- `DIVISION-BY-ZERO` - Arithmetic errors
- Array/sequence index errors

### Reader Errors
- `READER-ERROR` - Malformed syntax
- `END-OF-FILE` - Incomplete forms

### System Errors
- `PACKAGE-ERROR` - Package-related issues
- User-signaled errors via `ERROR`

### Warnings
- `WARNING` - Standard warnings (not errors)
- `STYLE-WARNING` - Style-related warnings

## Notes

- All test specifications use FiveAM test framework
- Property tests use randomized error generation for stability testing
- Tests verify server stability: no crashes, state preservation
- Error responses must include: type, message, backtrace
- Warnings must NOT set `isError: true`
- Critical for production stability and debuggability
