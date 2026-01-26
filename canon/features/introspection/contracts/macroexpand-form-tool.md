---
type: contract
name: macroexpand-form-tool
version: 0.1.0
---

# Macroexpand-Form Tool Contract

Expand a macro form to see the generated code. Supports both single-step and full expansion.

## Tool Definition

```json
{
  "name": "macroexpand-form",
  "description": "Expand a macro form to see the generated code. Useful for understanding what macros like DEFUN, LOOP, or WITH-* generate.",
  "inputSchema": {
    "type": "object",
    "required": ["form"],
    "properties": {
      "form": {
        "type": "string",
        "description": "The Lisp form to expand (as a string)"
      },
      "full": {
        "type": "boolean",
        "description": "If true, fully expand all macros recursively (default: false, single step)",
        "default": false
      }
    }
  }
}
```

## Input Processing

### Form Parsing

1. Read the `form` string using `read-from-string`
2. Handle read errors gracefully with informative messages
3. The form should be a valid Lisp expression

### Expansion Mode

| Mode | Function Used | Behavior |
|------|---------------|----------|
| Single step (`full: false`) | `macroexpand-1` | Expand only the outermost macro |
| Full (`full: true`) | `macroexpand` | Recursively expand until no macros remain |

## Output Format

### Success Response

```
Expansion of (MACRO-NAME ...):

(EXPANDED-FORM
  ...)
```

### Non-Macro Form

If the input is not a macro call:

```
Expansion of (FUNCTION-CALL ...):

(FUNCTION-CALL ...)

(Form is not a macro call)
```

### Read Error

```
Error reading form: <error-message>
```

## Output Formatting

- Pretty-printed with standard CL printer settings
- `*print-pretty*` = t
- `*print-case*` = :downcase (for readability)
- Indentation preserved for nested structures

## Examples

### Single-Step Expansion

Input:
```json
{"form": "(defun foo (x) (1+ x))"}
```

Output:
```
Expansion of (DEFUN FOO (X) (1+ X)):

(progn
  (eval-when (:compile-toplevel)
    (sb-c:%compiler-defun 'foo nil t))
  (sb-impl::%defun 'foo
                   (sb-int:named-lambda foo (x)
                     (block foo (1+ x)))))
```

### Full Expansion

Input:
```json
{"form": "(push item list)", "full": true}
```

Output:
```
Expansion of (PUSH ITEM LIST):

(setq list (cons item list))
```

### Loop Macro

Input:
```json
{"form": "(loop for i from 1 to 10 collect (* i i))"}
```

Output:
```
Expansion of (LOOP FOR I FROM 1 TO 10 COLLECT (* I I)):

(block nil
  (let ((i 1))
    (declare (type (and real number) i))
    (let ((#:loop-list-head nil)
          (#:loop-list-tail nil))
      (tagbody
        #:next-loop
        (when (> i 10) (go #:end-loop))
        ...))))
```

### WITH-* Macro

Input:
```json
{"form": "(with-open-file (s \"test.txt\") (read s))"}
```

Output:
```
Expansion of (WITH-OPEN-FILE (S "test.txt") (READ S)):

(let ((s (open "test.txt")))
  (unwind-protect
      (progn (read s))
    (when s (close s))))
```

### Non-Macro Form

Input:
```json
{"form": "(+ 1 2)"}
```

Output:
```
Expansion of (+ 1 2):

(+ 1 2)

(Form is not a macro call)
```

### Invalid Form

Input:
```json
{"form": "(defun"}
```

Output:
```
Error reading form: end of file on #<STRING-INPUT-STREAM>
```

## Error Response

When macroexpand fails:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{error message}"
    }
  ],
  "isError": true
}
```

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| `reader-error` | Malformed Lisp form | "Error reading form: {details}" |
| `end-of-file` | Incomplete form (unbalanced parens) | "Error reading form: end of file" |
| `package-error` | Unknown package prefix | "Package X not found" |
| `simple-error` | Macro signals error during expansion | Error from macro |

### Read Error Example

Input:
```json
{"form": "(defun"}
```

Response:
```
Error reading form: end of file on #<STRING-INPUT-STREAM>
```

### Macro Expansion Error

Some macros may signal errors during expansion (e.g., malformed syntax):

Input:
```json
{"form": "(loop for)"}
```

Response:
```
[ERROR] SB-INT:SIMPLE-PROGRAM-ERROR
LOOP requires a form after FOR.
```

## Implementation Notes

- Uses `macroexpand-1` for single-step, `macroexpand` for full
- Pretty-prints output for readability
- Detects non-macro forms by comparing input to expansion
- Package context uses `*package*` (typically CL-USER for MCP sessions)
- Some SBCL-specific expansions may include internal symbols
