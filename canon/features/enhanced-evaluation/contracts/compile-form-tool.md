---
type: contract
name: compile-form-tool
version: 0.1.0
---

# Compile-Form Tool Contract

Compile Common Lisp code without executing it. Catches compilation warnings,
type errors, and other issues that only appear at compile time.

## Tool Definition

```json
{
  "name": "compile-form",
  "description": "Compile Common Lisp code without executing it. Catches compilation warnings, type errors, and other issues that only appear at compile time. Useful for checking code correctness before evaluation.",
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

## Input Processing

### Code Compilation

1. Switch to specified package (default `CL-USER`)
2. Read forms from `code` string
3. Wrap in `with-compilation-unit` to batch warnings
4. Compile each form via `compile nil (lambda () form)`
5. Capture all compiler conditions (errors, warnings, notes)
6. Return compilation results without executing

### Package Handling

| Scenario | Behavior |
|----------|----------|
| Package not specified | Use `CL-USER` |
| Package not found | Return error |
| Multiple forms | Compile all in sequence |

## Output Format

### Success Response

```
Compilation successful
Warnings: 1
Errors: 0

WARNING: Undefined function: NONEXISTENT-FUNC
  in form: (DEFUN MY-FUNC () (NONEXISTENT-FUNC))
  severity: WARNING

Compiled 2 forms successfully
```

### Compilation Statistics

| Field | Description |
|-------|-------------|
| Warnings | Count of warnings emitted |
| Errors | Count of errors (compilation still may succeed) |
| Style-warnings | Count of style warnings |
| Notes | Count of informational notes |
| Forms-compiled | Number of toplevel forms compiled |

### Condition Reporting

Each compiler condition includes:

- **Severity**: `ERROR`, `WARNING`, `STYLE-WARNING`, `NOTE`
- **Message**: Human-readable description
- **Form**: The form being compiled (abbreviated)
- **Location**: Line/column if available

## Compilation Guarantees

### No Execution

**CRITICAL**: `compile-form` MUST NOT execute any user code.

```lisp
;; This should NOT print during compilation
(compile-form "(defun test () (print 'executed))")
;; Output: "Compilation successful" (no "EXECUTED" printed)
```

### Side Effect Safety

Compilation does not:
- Define functions in the image
- Set variable values
- Load files or systems
- Modify packages

After compilation, the session state is unchanged except for potential
compiler warnings being logged.

### Type Checking

The compiler performs type inference and checking:

```lisp
(compile-form "(defun bad-add (x) (+ x \"string\"))")
;; Output includes:
;; WARNING: Argument Y is a STRING, not a NUMBER.
```

## Examples

### Successful Compilation

Input:
```json
{
  "code": "(defun double (x) (* x 2))",
  "package": "CL-USER"
}
```

Output:
```
Compilation successful
Warnings: 0
Errors: 0

Compiled 1 form successfully
```

### Compilation with Warnings

Input:
```json
{
  "code": "(defun uses-undefined () (undefined-function 42))"
}
```

Output:
```
Compilation successful (with warnings)
Warnings: 1
Errors: 0

WARNING: Undefined function: UNDEFINED-FUNCTION
  in form: (DEFUN USES-UNDEFINED () (UNDEFINED-FUNCTION 42))
  severity: WARNING

Compiled 1 form successfully
```

### Type Error

Input:
```json
{
  "code": "(defun bad-call () (+ 1 \"two\"))"
}
```

Output:
```
Compilation successful (with warnings)
Warnings: 1
Errors: 0

WARNING: Forced to do GENERIC-+ (cost 10).
  Unable to do inline fixnum arithmetic (cost 2) because:
  The second argument is a STRING, not a FIXNUM.

Compiled 1 form successfully
```

### Syntax Error

Input:
```json
{
  "code": "(defun incomplete (x)"
}
```

Output:
```
Compilation failed
Errors: 1

ERROR: end of file on #<STRING-INPUT-STREAM>
  Could not read form from code string
```

## Implementation Notes

### Compilation Strategy

```lisp
(with-compilation-unit ()
  (handler-bind
    ((warning #'capture-warning)
     (style-warning #'capture-style-warning)
     (error #'capture-error))
    (loop for form in forms
          do (compile nil `(lambda () ,form)))))
```

### Condition Capture

Use `handler-bind` (not `handler-case`) to observe conditions without
preventing normal compilation flow. Collect conditions in a list for
final reporting.

### Deferred Warnings

Some warnings (like undefined functions) are deferred until the end of
the compilation unit. `with-compilation-unit` ensures these are captured.

## Verification Strategy

Tests should verify:

1. **No execution**: Code with side effects doesn't execute
2. **Warning capture**: All compiler warnings are reported
3. **Type checking**: Type mismatches generate warnings
4. **Syntax errors**: Malformed code is caught
5. **State preservation**: Session unchanged after compilation
6. **Package context**: Compilation uses correct package
