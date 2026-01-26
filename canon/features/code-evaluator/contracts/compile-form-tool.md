---
type: contract
name: compile-form-tool
version: 0.1.0
phase: B
---

# Compile-Form Tool Contract

Compiles Common Lisp code without executing it. Useful for catching type errors, style warnings, and other compilation-time issues before evaluation.

## Tool Definition

```json
{
  "name": "compile-form",
  "description": "Compile Common Lisp code without executing it. Catches compilation warnings, type errors, and other issues that only appear at compile time.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp code to compile"
      },
      "package": {
        "type": "string",
        "description": "Package context for compilation (default: CL-USER)"
      }
    }
  }
}
```

## Implementation

The code is wrapped in a lambda and compiled using `COMPILE`:

```lisp
(compile nil `(lambda () ,form))
```

This approach:
1. Allows compilation of any form, not just function definitions
2. Catches type inference warnings
3. Detects undefined functions/variables
4. Captures compiler notes and optimizations

## Output Format

### Success Response

```
Compilation successful.
No warnings or notes.
```

### With Warnings

```
Compilation successful.

Warnings (N):
  [STYLE] Undefined function: FOO
  Constant "string" conflicts with its asserted type NUMBER

Compiler notes (M):
  deleting unreachable code
```

### Compilation Failure

```
Compilation FAILED.

Errors (N):
  Read error: unexpected end of file
```

## Warning Categories

| Category | Description |
|----------|-------------|
| Warning | Standard compilation warnings |
| Style Warning | Code style issues (prefixed with `[STYLE]`) |
| Compiler Note | SBCL optimization notes |
| Error | Fatal compilation errors |

## Use Cases

1. **Type Checking**: Catch type mismatches before execution
2. **Code Review**: Identify style issues and dead code
3. **Syntax Validation**: Verify code structure without side effects
4. **Pre-flight Check**: Validate code before batch execution

## Comparison with evaluate-lisp

| Aspect | compile-form | evaluate-lisp |
|--------|--------------|---------------|
| Executes code | No | Yes |
| Catches type errors | Yes | Only at runtime |
| Shows compiler notes | Yes | No |
| Has side effects | No | Yes |
| Returns values | No | Yes |
