---
type: contract
name: describe-symbol-tool
version: 0.1.0
---

# Describe-Symbol Tool Contract

Get comprehensive information about a Lisp symbol including type, value,
documentation, arglist, and source location.

## Tool Definition

```json
{
  "name": "describe-symbol",
  "description": "Get comprehensive information about a Lisp symbol including its type, value, documentation, arglist, and source location. Uses SBCL introspection for detailed information.",
  "inputSchema": {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "type": "string",
        "description": "Symbol name to describe"
      },
      "package": {
        "type": "string",
        "description": "Package name (defaults to CL-USER)"
      }
    }
  }
}
```

## Input Processing

### Symbol Resolution

1. Convert `name` to uppercase (Lisp symbols are case-insensitive)
2. Find package by `package` parameter or default to `CL-USER`
3. Look up symbol in package using `find-symbol`
4. Return error message if package or symbol not found

### Package Handling

| Scenario | Behavior |
|----------|----------|
| Package not specified | Use `CL-USER` |
| Package not found | Return "Package X not found" |
| Symbol not found | Return "Symbol X not found in package Y" |

## Output Format

### Success Response

```
PACKAGE::SYMBOL-NAME [TYPE]
  Arglist: (arg1 arg2 &optional opt)
  Value: <printed-value>
  Documentation:
    <docstring>
  Source: /path/to/file.lisp:12345
```

### Information Included

| Field | When Shown | Source |
|-------|------------|--------|
| Type | Always | `symbol-type-info` |
| Arglist | Functions/macros | `sb-introspect:function-lambda-list` |
| Value | Bound variables | `symbol-value` with print limits |
| Documentation | If docstring exists | `documentation` |
| Source | If source available | `sb-introspect:find-definition-sources-by-name` |

### Value Printing

Variable values are printed with safety limits:
- `*print-length*` = 20
- `*print-level*` = 3
- `*print-circle*` = t

If printing errors, shows `<error printing value>`.

## Symbol Type Classification

```lisp
(defun symbol-type-info (sym)
  (cond
    ((and (fboundp sym) (macro-function sym)) :macro)
    ((and (fboundp sym) (typep (fdefinition sym) 'generic-function)) :generic-function)
    ((fboundp sym) :function)
    ((find-class sym nil) :class)
    ((boundp sym) :variable)
    (t :symbol)))
```

## Examples

### Function Query

Input:
```json
{"name": "mapcar", "package": "CL"}
```

Output:
```
COMMON-LISP::MAPCAR [FUNCTION]
  Arglist: (FUNCTION LIST &REST MORE-LISTS)
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return list of FUNCTION return values.
  Source: SYS:SRC;CODE;LIST.LISP:51454
```

### Variable Query

Input:
```json
{"name": "*print-base*", "package": "CL"}
```

Output:
```
COMMON-LISP::*PRINT-BASE* [VARIABLE]
  Value: 10
  Documentation:
    The output base for rationals.
```

### Unknown Symbol

Input:
```json
{"name": "nonexistent-symbol"}
```

Output:
```
Symbol NONEXISTENT-SYMBOL not found in package CL-USER (status: NIL)
```

## Error Response

When describe-symbol fails:

```json
{
  "content": [
    {
      "type": "text",
      "text": "{error message}"
    }
  ],
  "isError": false
}
```

Note: Symbol lookup failures return descriptive messages but `isError: false` since they are informational responses rather than execution errors.

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| Package not found | Invalid package name | "Package X not found" |
| Symbol not found | Symbol doesn't exist | "Symbol X not found in package Y (status: NIL)" |

### Package Not Found

Input:
```json
{"name": "foo", "package": "NONEXISTENT"}
```

Response:
```
Package NONEXISTENT not found
```

### Symbol Not Found

Input:
```json
{"name": "nonexistent-symbol"}
```

Response:
```
Symbol NONEXISTENT-SYMBOL not found in package CL-USER (status: NIL)
```

## Implementation Notes

- Uses `sb-introspect` for SBCL-specific information
- Source locations may be SBCL internal paths (SYS:...) for built-ins
- Macro arglists are available via `function-lambda-list`
