---
type: contract
name: who-calls-tool
version: 0.1.0
---

# Who-Calls Tool Contract

Find all functions that call a given function using SBCL's cross-reference database.

## Tool Definition

```json
{
  "name": "who-calls",
  "description": "Find all functions that call a given function. Uses SBCL's cross-reference database to discover callers.",
  "inputSchema": {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "type": "string",
        "description": "Function name to find callers of"
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

1. Convert `name` to uppercase
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

### Success Response (with callers)

```
Functions that call PACKAGE::SYMBOL-NAME:

  PACKAGE-1::CALLER-A
  PACKAGE-2::CALLER-B
  PACKAGE-3::CALLER-C
```

### Success Response (no callers)

```
No callers found for PACKAGE::SYMBOL-NAME
```

### Result Entry Format

Each caller shows its fully qualified name: `PACKAGE::NAME`

### Result Ordering

- Sorted alphabetically by symbol name
- Duplicates removed

## Examples

### Finding Callers of a Standard Function

Input:
```json
{"name": "mapcar", "package": "CL"}
```

Output:
```
Functions that call COMMON-LISP::MAPCAR:

  ALEXANDRIA::CURRY
  ALEXANDRIA::MAPPEND
  CL-MCP-SERVER.TOOLS::FORMAT-TOOLS-LIST
  ...
```

### Function with No Callers

Input:
```json
{"name": "my-unused-function", "package": "MY-PACKAGE"}
```

Output:
```
No callers found for MY-PACKAGE::MY-UNUSED-FUNCTION
```

### Unknown Symbol

Input:
```json
{"name": "nonexistent"}
```

Output:
```
Symbol NONEXISTENT not found in package CL-USER (status: NIL)
```

## Implementation Notes

- Uses `sb-introspect:who-calls` for caller discovery
- Cross-reference data is populated during compilation
- Functions compiled without optimization may have incomplete xref data
- Built-in functions may show many callers from loaded systems
