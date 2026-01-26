---
feature: introspection
verification_status: specification-complete
last_updated: 2026-01-26
---

# Introspection Verification

Test suite for the introspection feature, covering contracts, scenarios, and properties.

## Test Organization

```
verification/
├── contracts/          # Individual contract tests
│   ├── describe-symbol.test.md
│   ├── apropos-search.test.md
│   └── ... (other tool contracts)
├── scenarios/          # Full scenario workflows
│   ├── symbol-inspection.test.md
│   └── ... (other scenarios)
├── properties/         # Property-based tests
│   ├── read-only-guarantee.test.md
│   └── ... (other properties)
└── README.md          # This file
```

## Running Tests

### Quick Smoke Test

Run basic introspection tests:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-smoke-suite)"
```

Duration: ~15 seconds

### Contract Tests

Test each tool contract in isolation:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-contract-suite)"
```

Duration: ~45 seconds

### Scenario Tests

Test complete introspection workflows:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-scenario-suite)"
```

Duration: ~1 minute

### Property Tests

Run property-based tests with default trials (50):

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-property-suite)"
```

Duration: ~3 minutes

### Extended Property Tests

Run with 200 trials for thorough verification:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(setf cl-quickcheck:*num-trials* 200)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-property-suite)"
```

Duration: ~15 minutes (CI only)

### Full Suite

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:introspection-suite)"
```

Duration: ~5 minutes

## Coverage

### Contracts Covered

| Contract | Test File | Status |
|----------|-----------|--------|
| describe-symbol-tool | contracts/describe-symbol.test.md | ✅ |
| apropos-search-tool | contracts/apropos-search.test.md | ✅ |
| who-calls-tool | ❌ Not yet implemented | ⏳ |
| who-references-tool | ❌ Not yet implemented | ⏳ |
| macroexpand-form-tool | ❌ Not yet implemented | ⏳ |
| validate-syntax-tool | ❌ Not yet implemented | ⏳ |

### Scenarios Covered

| Scenario | Test File | Status |
|----------|-----------|--------|
| symbol-inspection | scenarios/symbol-inspection.test.md | ✅ |
| symbol-discovery | ❌ Not yet implemented | ⏳ |
| cross-reference | ❌ Not yet implemented | ⏳ |
| macro-understanding | ❌ Not yet implemented | ⏳ |
| syntax-validation | ❌ Not yet implemented | ⏳ |

### Properties Covered

| Property | Test File | Status |
|----------|-----------|--------|
| read-only-guarantee | properties/read-only-guarantee.test.md | ✅ |
| symbol-information-accuracy | ❌ Not yet implemented | ⏳ |
| cross-reference-correctness | ❌ Not yet implemented | ⏳ |
| macro-expansion-correctness | ❌ Not yet implemented | ⏳ |
| sbcl-introspection-guarantees | ❌ Not yet implemented | ⏳ |
| syntax-validation-safety | ❌ Not yet implemented | ⏳ |
| symbol-discovery-completeness | ❌ Not yet implemented | ⏳ |

## Test Implementation Status

Current implementation: **Partial Specifications, Code Pending**

These test files are **specifications** written in markdown with embedded Lisp test code. They document:
- What to test
- How to test it
- Expected results

**Current status**: Core tools (describe-symbol, apropos-search) have complete specs. Additional tools need test specifications.

**Next step**:
1. Complete test specifications for remaining tools
2. Implement actual test files in `tests/` directory:
```
tests/
├── introspection/
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

## Introspection Tools

The introspection feature provides these tools:

### Symbol Information
- `describe-symbol` - Comprehensive symbol information (type, value, arglist, docs, source)
- `apropos-search` - Find symbols matching pattern with filtering

### Cross-Reference
- `who-calls` - Find all functions calling a given function
- `who-references` - Find all code referencing a variable

### Code Analysis
- `macroexpand-form` - Expand macros to understand transformations
- `validate-syntax` - Check if code is syntactically valid without evaluating

## Key Properties

### Read-Only Guarantee

All introspection tools MUST:
- Never modify session state
- Never call functions as side effects
- Never change variable values
- Return consistent results on repeated calls
- Handle nonexistent symbols safely

### Information Accuracy

All introspection results MUST:
- Reflect current session state accurately
- Include all relevant information (type, docs, etc.)
- Use SBCL introspection APIs correctly
- Handle package context properly

## Notes

- All test specifications use FiveAM test framework
- Property tests verify read-only guarantee extensively
- Tests cover both system symbols (CL package) and user-defined symbols
- SBCL-specific introspection is acceptable (sb-introspect)
- Cross-reference tools use SBCL's xref database
- Critical that introspection is truly read-only for safety
