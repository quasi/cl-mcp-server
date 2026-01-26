---
feature: error-intelligence
verification_status: specified
last_updated: 2026-01-26
---

# Error Intelligence Verification

Test specifications for the error-intelligence feature.

## Purpose

Verification tests ensure the error-intelligence tools (describe-last-error, get-backtrace)
capture complete error information and preserve diagnostic state correctly.

## Test Organization

```
verification/
├── contracts/           # Contract conformance tests
│   └── describe-last-error-tool.test.md
├── properties/          # Property-based tests
│   ├── error-capture-completeness.test.md
│   ├── backtrace-accuracy.test.md
│   ├── error-state-preservation.test.md
│   └── restart-information-completeness.test.md
└── scenarios/           # Scenario-based tests (optional)
```

## Running Tests

From the MCP server test suite:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:error-intelligence-suite)"
```

## Test Requirements

### Contract Tests

- Error type correctly identified
- Error message captured
- Backtrace present with frames
- Restarts available and listed
- No error case handled gracefully

### Property Tests

- All errors produce diagnostic information
- Error types are specific (not generic)
- Backtraces show complete call chain
- Restarts correctly identified
- Error state preserved across tool calls

## Coverage Goals

- All common error types tested
- Nested call chains verified
- Custom error messages preserved
- Restart information complete
- State management correct
