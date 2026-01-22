# Error Handling Feature Index

**Purpose**: Condition capture and structured error reporting.

**Status**: draft
**Priority**: P0 (required for stability)

## Quick Navigation

| Need to... | Read |
|------------|------|
| Format condition reports | `contracts/condition-report.md` |
| Test error scenarios | `scenarios/evaluation-errors.md` |
| Understand Common Lisp conditions | `vocabulary.md` |

## Contents

### Contracts (1)

| File | Purpose | Lines | Key Points |
|------|---------|-------|------------|
| `contracts/condition-report.md` | Error formatting and reporting | TBD | Condition type, message, backtrace formatting |

### Scenarios (1)

| File | Purpose | Test Cases |
|------|---------|------------|
| `scenarios/evaluation-errors.md` | Error handling cases | Runtime errors, type errors, undefined functions, reader errors |

### Vocabulary (1)

`vocabulary.md` - Error handling terms:
- **Condition**: Lisp's exception mechanism
- **Handler**: Code that responds to conditions
- **Backtrace**: Stack trace at point of error
- **Muffle**: Prevent condition from propagating

## Condition Hierarchy

Common Lisp's condition system:

```
condition
├── serious-condition
│   ├── error
│   │   ├── type-error
│   │   ├── program-error
│   │   │   ├── undefined-function
│   │   │   └── control-error
│   │   ├── arithmetic-error
│   │   │   ├── division-by-zero
│   │   │   └── floating-point-overflow
│   │   ├── file-error
│   │   ├── package-error
│   │   ├── stream-error
│   │   │   └── end-of-file
│   │   ├── cell-error
│   │   │   ├── unbound-variable
│   │   │   └── undefined-function
│   │   └── reader-error
│   └── storage-condition
└── warning
    ├── style-warning
    └── simple-warning
```

## Handling Strategy

### Warnings

**Strategy**: Record and muffle (don't stop evaluation).

**Implementation**:
```lisp
(defvar *warnings* nil)

(defun evaluate-with-warnings (form)
  (let ((*warnings* nil))
    (handler-bind ((warning (lambda (c)
                              (push c *warnings*)
                              (muffle-warning c))))
      (values (eval form) *warnings*))))
```

**Result Format**:
```
[warnings]
STYLE-WARNING: implicit return of NIL
SIMPLE-WARNING: unused variable X

=> {result}
```

### Errors

**Strategy**: Catch and report (prevent server crash).

**Implementation**:
```lisp
(defun safe-evaluate (form)
  (handler-case (eval form)
    (error (c)
      (make-error-response c))))
```

**Result Format**:
```
[ERROR] {CONDITION-TYPE}
{condition message}

[Backtrace]
{formatted backtrace}
```

### Serious Conditions

**Strategy**: Catch like errors (includes `storage-condition`).

**Rationale**: Even non-error serious conditions must not crash server (INV-002).

## Error Response Format

### Structure

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] DIVISION-BY-ZERO\narithmetic error...\n\n[Backtrace]\n..."
    }
  ],
  "isError": true
}
```

**Key**: `isError: true` flag distinguishes errors from successful results.

### Text Format

```
[ERROR] {CONDITION-TYPE-NAME}
{formatted condition message}

[Backtrace]
{frame 0}
{frame 1}
...
{frame N}
```

### Backtrace Formatting

**Goals**:
- Show relevant user frames (not internal evaluator frames)
- Limit depth to ~20 frames (avoid overwhelming output)
- Show function names and arguments

**Format per frame**:
```
N: (FUNCTION-NAME ARG1 ARG2 ...)
```

**Example**:
```
[Backtrace]
0: (/ 1 0)
1: (COMPUTE-AVERAGE (1 2 0 3))
2: (PROCESS-DATA #<VECTOR ...>)
```

### Condition Type Preservation

**Critical (INV-006)**: Always include condition type name.

**Good**:
```
[ERROR] UNDEFINED-FUNCTION
The function FOO is undefined.
```

**Bad**:
```
[ERROR]
The function FOO is undefined.
```

**Rationale**: Type name (e.g., `UNDEFINED-FUNCTION` vs `TYPE-ERROR`) provides diagnostic context.

## Example Scenarios

### Division by Zero

**Input**:
```json
{
  "code": "(/ 1 0)"
}
```

**Output**:
```
[ERROR] DIVISION-BY-ZERO
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).

[Backtrace]
0: (/ 1 0)
```

### Undefined Function

**Input**:
```json
{
  "code": "(foo 42)"
}
```

**Output**:
```
[ERROR] UNDEFINED-FUNCTION
The function COMMON-LISP-USER::FOO is undefined.

[Backtrace]
0: (FOO 42)
```

### Type Error

**Input**:
```json
{
  "code": "(car 42)"
}
```

**Output**:
```
[ERROR] TYPE-ERROR
The value 42 is not of type LIST.

[Backtrace]
0: (CAR 42)
```

### Reader Error

**Input**:
```json
{
  "code": "(defun foo ("
}
```

**Output**:
```
[ERROR] END-OF-FILE
end of file on #<STRING-INPUT-STREAM>

[Backtrace]
0: (READ-FROM-STRING "(defun foo (")
```

**Note**: Reader errors occur before evaluation, so backtrace is minimal.

### Warning (Non-Error)

**Input**:
```json
{
  "code": "(defun foo () (let ((x 10))))"
}
```

**Output**:
```
[warnings]
STYLE-WARNING: The variable X is defined but never used.

=> FOO
```

**Note**: Evaluation succeeds (`isError: false`), but warnings are reported.

## Implementation Patterns

### Top-Level Handler

```lisp
(defun handle-evaluate-request (request)
  (handler-case
      (let ((result (safe-evaluate (request-code request))))
        (make-success-response result))
    (serious-condition (c)
      ;; Last resort: even if safe-evaluate fails
      (make-error-response c))))
```

**Two levels of protection**:
1. `safe-evaluate` catches user code errors
2. Top-level `handler-case` catches evaluator bugs

### Backtrace Extraction

```lisp
(defun format-backtrace (condition)
  #+sbcl
  (with-output-to-string (s)
    (sb-debug:print-backtrace :stream s :count 20))
  #-sbcl
  (format nil "Backtrace not available on this implementation"))
```

**Implementation-specific**: Backtrace APIs vary by Lisp.

### Warning Collection

```lisp
(defvar *warnings* nil)

(defun collect-warning (condition)
  (push (format-warning condition) *warnings*)
  (muffle-warning condition))

(defun evaluate-with-warnings (form)
  (let ((*warnings* nil))
    (handler-bind ((warning #'collect-warning))
      (eval form))))
```

## Dependencies

**Depends on**:
- `mcp-protocol` (error response format)
- `core/foundation/vocabulary.md` (Condition)
- `core/foundation/invariants.md` (INV-002: stability, INV-006: type preservation)

**Depended on by**:
- `code-evaluator` (uses error formatting for evaluation errors)

## Verification

From `verification/` directory:
- Error type preservation tests
- Backtrace formatting tests
- Warning collection tests
- Server stability tests (errors don't crash server)

## Common Issues

### Issue: Server crashes on error

**Symptom**: Server terminates when user code signals error.

**Cause**: Missing `handler-case` wrapper (violates INV-002).

**Solution**: Wrap all evaluation in `handler-case` at server loop level.

### Issue: Condition type lost

**Symptom**: Error response shows message but not type name.

**Cause**: Using `princ-to-string` instead of capturing `(type-of condition)`.

**Solution**: Explicitly format `[ERROR] {TYPE}` header.

### Issue: Backtraces too verbose

**Symptom**: Hundreds of frames, mostly internal.

**Cause**: Not limiting frame count.

**Solution**: Limit to ~20 frames, skip internal evaluator frames.

### Issue: Warnings stop evaluation

**Symptom**: `style-warning` causes evaluation to fail.

**Cause**: Not muffling warnings.

**Solution**: Use `muffle-warning` in warning handler.

## Design Decisions

### Decision: Warnings Don't Fail Evaluation

**Choice**: Warnings are recorded but evaluation continues.

**Rationale**:
- Matches REPL behavior (warnings are informational)
- Style warnings are common and benign
- User can see warnings in response

**Alternative**: Could make warnings errors (rejected as too strict).

### Decision: Include Backtrace in Error Response

**Choice**: Always include formatted backtrace for errors.

**Rationale**:
- Essential for debugging
- Claude can use backtrace to diagnose issues
- Cost is minimal (already in error path)

**Alternative**: Could omit or make optional (rejected as less useful).

### Decision: Preserve Condition Type

**Choice**: Always report condition type name (INV-006).

**Rationale**:
- Different types require different fixes
- Type name is canonical diagnostic info
- Lisp's condition system is type-based

**Alternative**: Just show message (rejected as loses information).

---

**Last Updated**: 2026-01-22
**Status**: draft
**Next Steps**: Implement condition-report contract, test all error types
