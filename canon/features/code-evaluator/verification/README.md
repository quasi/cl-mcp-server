---
feature: code-evaluator
verification_status: specifications_complete
last_updated: 2026-01-26
---

# Code Evaluator Verification

Test suite for the code-evaluator feature, covering contracts, scenarios, and properties.

## Test Organization

```
verification/
├── contracts/          # Individual contract tests
│   ├── evaluate-lisp-tool.test.md
│   ├── configure-limits-tool.test.md
│   ├── compile-form-tool.test.md
│   └── time-execution-tool.test.md
├── scenarios/          # Full scenario workflows
│   ├── basic-evaluation.test.md
│   ├── definitions.test.md
│   ├── output-capture.test.md
│   └── timeout-handling.test.md
├── properties/         # Property-based tests
│   ├── evaluation-isolation.test.md
│   ├── condition-handling.test.md
│   ├── output-capture.test.md
│   ├── state-persistence.test.md
│   └── timeout-protection.test.md
└── README.md          # This file
```

## Running Tests

### Quick Smoke Test

Run basic evaluation and contract tests:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-smoke-suite)"
```

Duration: ~15 seconds

### Contract Tests

Test each tool contract in isolation:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-contract-suite)"
```

Duration: ~45 seconds

Includes:
- evaluate-lisp tool (basic evaluation, output capture, error handling)
- configure-limits tool (timeout and output limit configuration)
- compile-form tool (compilation without execution) [Phase B]
- time-execution tool (performance profiling) [Phase B]

### Scenario Tests

Test complete workflows:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-scenario-suite)"
```

Duration: ~1 minute

Includes:
- Basic evaluation workflow (arithmetic, strings, lists, multiple values)
- Function and variable definitions (persistence across evaluations)
- Output stream capture (stdout, stderr, warnings separation)
- Timeout handling (infinite loops, partial output, server stability)

### Property Tests

Run property-based tests with default trials (100):

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-property-suite)"
```

Duration: ~3 minutes

Includes:
- Evaluation isolation (session boundaries, state protection)
- Condition handling (errors, warnings, backtrace)
- Output capture (stream separation, section ordering)
- State persistence (definitions, modifications, redefining)
- Timeout protection (slow code, server stability)

### Extended Property Tests

Run with 1000 trials for thorough verification:

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(setf *property-test-trials* 1000)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-property-suite)"
```

Duration: ~30 minutes (CI only)

### Full Suite

```bash
sbcl --eval "(ql:quickload :cl-mcp-server/tests)" \
     --eval "(fiveam:run! 'cl-mcp-server-tests:code-evaluator-suite)"
```

Duration: ~5 minutes

## Coverage

### Contracts Covered

| Contract | Test File | Status | Phase |
|----------|-----------|--------|-------|
| evaluate-lisp-tool | contracts/evaluate-lisp-tool.test.md | ✅ | A |
| configure-limits-tool | contracts/configure-limits-tool.test.md | ✅ | A |
| compile-form-tool | contracts/compile-form-tool.test.md | ✅ | B |
| time-execution-tool | contracts/time-execution-tool.test.md | ✅ | B |

### Scenarios Covered

| Scenario | Test File | Status |
|----------|-----------|--------|
| basic-evaluation | scenarios/basic-evaluation.test.md | ✅ |
| definitions | scenarios/definitions.test.md | ✅ |
| output-capture | scenarios/output-capture.test.md | ✅ |
| timeout-handling | scenarios/timeout-handling.test.md | ✅ |

### Properties Covered

| Property | Test File | Status |
|----------|-----------|--------|
| evaluation-isolation | properties/evaluation-isolation.test.md | ✅ |
| condition-handling | properties/condition-handling.test.md | ✅ |
| output-capture | properties/output-capture.test.md | ✅ |
| state-persistence | properties/state-persistence.test.md | ✅ |
| timeout-protection | properties/timeout-protection.test.md | ✅ |

## Test Implementation Status

Current implementation: **Specifications Complete, Code Pending**

These test files are **specifications** written in markdown with embedded Lisp test code. They document:
- What to test
- How to test it
- Expected results
- Edge cases and properties

**Next step**: Implement actual test files in `tests/` directory:
```
tests/
├── code-evaluator/
│   ├── contracts-test.lisp
│   ├── scenarios-test.lisp
│   └── properties-test.lisp
```

## Key Test Patterns

### Contract Tests
- Test individual tool inputs and outputs
- Verify parameter validation
- Check error responses for invalid inputs
- Confirm output format compliance

### Scenario Tests
- Test complete workflows from start to finish
- Verify state transitions
- Record message transcripts
- Check request-response pairing

### Property Tests
- Use randomized test generation
- Test invariants across many examples
- Support shrinking to find minimal failures
- Cover edge cases automatically

## Invariants Verified

These tests verify the following system invariants:

- **INV-002**: Server Stability - Server never terminates due to evaluation errors or timeouts
- **INV-003**: Session State Persistence - Definitions persist within sessions
- **INV-004**: Output Stream Separation - Stdout, stderr, warnings, and values are distinguishable
- **INV-006**: Condition Type Preservation - Error types are preserved and reported

## Notes

### Test Helper Functions

Tests assume these helper functions exist:

```lisp
;; Session management
(make-test-session) → session
(cleanup-test-session session) → void
(evaluate-in-session session code) → result

;; Request/response
(send-request server request) → response
(send-tool-call session tool-name args) → response

;; Response parsing
(result-response-p response) → boolean
(error-response-p response) → boolean
(result-field response field) → value
(result-content-text response) → string
(response-id response) → id
(error-code response) → code

;; Configuration
(configure-session-timeout session seconds) → void
```

### Test Framework

- All tests use **FiveAM** test framework
- Property tests use randomized generation
- Tests are isolated (independent sessions)
- No test should take more than 30 seconds individually

### Phase Requirements

- **Phase A** tests: Core evaluation functionality (evaluate-lisp, configure-limits)
- **Phase B** tests: Additional tools (compile-form, time-execution)

Phase A tests must pass before Phase B implementation begins.

### Timeout Considerations

- Timeout tests use low timeouts (1-2 seconds) for speed
- Precision tests allow ±0.5 second variance
- Resource leak tests include brief cleanup pauses
- Disabled timeout tests use finite operations for safety

## CI Integration

Recommended CI test stages:

1. **Smoke** (30 sec): Contract smoke tests
2. **Core** (2 min): All contracts + basic scenarios
3. **Full** (5 min): All tests with default property trials
4. **Extended** (30 min, nightly): Property tests with 1000 trials

## Related Documentation

- [Feature Overview](../INDEX.md)
- [Tool Contracts](../contracts/)
- [Scenarios](../scenarios/)
- [Properties](../properties/)
- [Implementation Guide](../../../AGENT.md)
