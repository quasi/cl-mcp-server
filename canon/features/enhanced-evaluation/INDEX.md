# Enhanced Evaluation Feature

**Phase B: Enhanced Evaluation Tools**

Extends basic code evaluation with compilation checking, timing analysis, and
detailed profiling capabilities.

## Navigation

### Foundation
- [Feature Definition](./feature.yaml) - Feature metadata and dependencies
- [Vocabulary](./vocabulary.md) - Domain terminology

### Contracts
- [compile-form-tool](./contracts/compile-form-tool.md) - Compile without executing
- [time-execution-tool](./contracts/time-execution-tool.md) - Detailed timing and profiling

### Properties
- [compilation-safety](./properties/compilation-safety.md) - No execution during compilation
- [timing-accuracy](./properties/timing-accuracy.md) - Accurate performance measurements

### Scenarios
- (To be created) - End-to-end usage workflows

### Verification
- [README](./verification/README.md) - Verification test overview

## Key Capabilities

### Compilation Checking
- Compile code without side effects
- Capture warnings and errors
- Type checking before execution
- Safe pre-flight validation

### Performance Profiling
- Wall-clock and CPU time measurement
- Memory allocation tracking
- GC time separation
- Minimal overhead timing

## Dependencies

### Requires
- **core**: Foundation vocabulary (Session, Evaluation, Condition)
- **session-management**: Package context for compilation/execution
- **code-evaluator**: Base evaluation infrastructure

### Used By
- **profiling**: Builds on timing capabilities
- **error-intelligence**: Enhanced error context

## Tool Summary

| Tool | Purpose | Safety |
|------|---------|--------|
| compile-form | Check code for errors | Read-only, no execution |
| time-execution | Measure performance | Executes code |

## Status

- **Vocabulary**: Stable
- **Contracts**: Stable
- **Properties**: In progress
- **Scenarios**: Not started
- **Verification**: Not started

## See Also

- [Core Vocabulary](../../core/foundation/vocabulary.md)
- [Code Evaluator Feature](../code-evaluator/INDEX.md)
- [Session Management Feature](../session-management/INDEX.md)
