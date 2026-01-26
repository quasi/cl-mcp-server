---
feature: mcp-protocol
verification_status: complete
last_updated: 2026-01-26
---

# MCP Protocol Verification

Test suite for the MCP protocol feature, covering contracts, scenarios, and properties.

## Test Organization

```
verification/
├── contracts/          # Individual contract tests
│   ├── initialization.test.md
│   └── tools-list.test.md
├── scenarios/          # Full scenario workflows
│   ├── initialization-handshake.test.md
│   └── malformed-request.test.md
├── properties/         # Property-based tests
│   ├── protocol-conformance.test.md
│   └── request-response-guarantee.test.md
└── README.md          # This file
```

## Running Tests

### Quick Smoke Test

Run initialization and basic protocol tests:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-smoke-suite)"
```

Duration: ~10 seconds

### Contract Tests

Test each contract in isolation:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-contract-suite)"
```

Duration: ~30 seconds

### Scenario Tests

Test complete workflows:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-scenario-suite)"
```

Duration: ~45 seconds

### Property Tests

Run property-based tests with default examples (1000):

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-property-suite)"
```

Duration: ~2 minutes

### Extended Property Tests

Run with 10000 examples for thorough verification:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(setf cl-quickcheck:*num-trials* 10000)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-property-suite)"
```

Duration: ~20 minutes (CI only)

### Full Suite

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:mcp-protocol-suite)"
```

Duration: ~3 minutes

## Coverage

### Contracts Covered

| Contract | Test File | Status |
|----------|-----------|--------|
| initialization | contracts/initialization.test.md | ✅ |
| tools-list | contracts/tools-list.test.md | ✅ |
| tools-call | ❌ Not yet implemented | ⏳ |
| transport | ❌ Not yet implemented | ⏳ |

### Scenarios Covered

| Scenario | Test File | Status |
|----------|-----------|--------|
| initialization-handshake | scenarios/initialization-handshake.test.md | ✅ |
| malformed-request | scenarios/malformed-request.test.md | ✅ |

### Properties Covered

| Property | Test File | Status |
|----------|-----------|--------|
| protocol-conformance | properties/protocol-conformance.test.md | ✅ |
| request-response-guarantee | properties/request-response-guarantee.test.md | ✅ |
| initialization-state-machine | ❌ Not yet implemented | ⏳ |
| error-response-stability | ❌ Not yet implemented | ⏳ |
| message-integrity | ❌ Not yet implemented | ⏳ |

## Test Implementation Status

Current implementation: **Specifications Complete, Code Pending**

These test files are **specifications** written in markdown with embedded Lisp test code. They document:
- What to test
- How to test it
- Expected results

**Next step**: Implement actual test files in `tests/` directory:
```
tests/
├── mcp-protocol/
│   ├── contracts-test.lisp
│   ├── scenarios-test.lisp
│   └── properties-test.lisp
```

## Notes

- All test specifications use FiveAM test framework
- Property tests use cl-quickcheck or similar
- Tests assume test helper functions: `make-initialized-test-server`, `send-request`, etc.
- Concurrent tests require bordeaux-threads
