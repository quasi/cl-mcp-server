---
type: contract
name: who-references-tool
version: 0.1.0
---

# Who-References Tool Contract

Find all code that references (reads) a given variable using SBCL's cross-reference database.

## Tool Definition

```json
{
  "name": "who-references",
  "description": "Find all code that references (reads) a given variable. Uses SBCL's cross-reference database.",
  "inputSchema": {
    "type": "object",
    "required": ["name"],
    "properties": {
      "name": {
        "type": "string",
        "description": "Variable name to find references to"
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

### Success Response (with references)

```
Code that references PACKAGE::*VARIABLE-NAME*:

  PACKAGE-1::FUNCTION-A
  PACKAGE-2::FUNCTION-B
  PACKAGE-3::FUNCTION-C
```

### Success Response (no references)

```
No references found for PACKAGE::*VARIABLE-NAME*
```

### Result Entry Format

Each reference shows the fully qualified name of the function containing the reference: `PACKAGE::NAME`

### Result Ordering

- Sorted alphabetically by symbol name
- Duplicates removed

## Examples

### Finding References to a Standard Variable

Input:
```json
{"name": "*print-base*", "package": "CL"}
```

Output:
```
Code that references COMMON-LISP::*PRINT-BASE*:

  CL-MCP-SERVER.EVALUATOR::SAFE-PRINT-VALUE
  FORMAT::FORMAT-DIRECTIVE
  ...
```

### Variable with No References

Input:
```json
{"name": "*my-unused-var*", "package": "MY-PACKAGE"}
```

Output:
```
No references found for MY-PACKAGE::*MY-UNUSED-VAR*
```

### Unknown Symbol

Input:
```json
{"name": "*nonexistent*"}
```

Output:
```
Symbol *NONEXISTENT* not found in package CL-USER (status: NIL)
```

## Error Response

When who-references fails:

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

Note: Lookup failures return descriptive messages but `isError: false` since they are informational responses.

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| Package not found | Invalid package name | "Package X not found" |
| Symbol not found | Symbol doesn't exist | "Symbol X not found in package Y (status: NIL)" |

### Package Not Found

Input:
```json
{"name": "*var*", "package": "NONEXISTENT"}
```

Response:
```
Package NONEXISTENT not found
```

## Implementation Notes

- Uses `sb-introspect:who-references` for reference discovery
- Tracks read access to special variables
- Cross-reference data is populated during compilation
- Does not track lexical variable references (only special/dynamic variables)
- Related functions: `who-binds` (finds binding sites), `who-sets` (finds write sites)
