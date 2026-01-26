---
type: contract
name: apropos-search-tool
version: 0.1.0
---

# Apropos-Search Tool Contract

Search for symbols matching a pattern with optional type and package filtering.

## Tool Definition

```json
{
  "name": "apropos-search",
  "description": "Search for symbols matching a pattern. Returns symbol names, types, and packages. Useful for discovering available functions, variables, and classes.",
  "inputSchema": {
    "type": "object",
    "required": ["pattern"],
    "properties": {
      "pattern": {
        "type": "string",
        "description": "Search pattern (case-insensitive substring)"
      },
      "package": {
        "type": "string",
        "description": "Limit search to this package (optional)"
      },
      "type": {
        "type": "string",
        "description": "Filter by type: function, macro, variable, class, generic-function",
        "enum": ["function", "macro", "variable", "class", "generic-function"]
      }
    }
  }
}
```

## Input Processing

### Pattern Matching

- Case-insensitive substring match against symbol names
- Uses `search` with `:test #'char-equal`
- Empty pattern matches all symbols (not recommended)

### Package Scope

| Scenario | Search Scope |
|----------|--------------|
| Package not specified | External symbols of all packages |
| Package specified | All symbols in that package |
| Package not found | Return "Package X not found" |

### Type Filtering

If `type` is specified, only symbols matching that type are returned.

## Output Format

### Success Response

```
Found N symbols matching 'PATTERN':

  PACKAGE-1::SYMBOL-A [FUNCTION]
  PACKAGE-2::SYMBOL-B [MACRO]
  PACKAGE-3::SYMBOL-C [VARIABLE]
```

### Result Entry Format

Each matching symbol shows:
- Full qualified name: `PACKAGE::NAME`
- Type classification in brackets

### Result Ordering

- Sorted alphabetically by symbol name
- Duplicates removed (same name+package)

## Examples

### Basic Search

Input:
```json
{"pattern": "map"}
```

Output:
```
Found 12 symbols matching 'map':

  COMMON-LISP::MAP [FUNCTION]
  COMMON-LISP::MAPC [FUNCTION]
  COMMON-LISP::MAPCAN [FUNCTION]
  COMMON-LISP::MAPCAR [FUNCTION]
  COMMON-LISP::MAPCON [FUNCTION]
  COMMON-LISP::MAPHASH [FUNCTION]
  COMMON-LISP::MAPL [FUNCTION]
  COMMON-LISP::MAPLIST [FUNCTION]
  ...
```

### Package-Scoped Search

Input:
```json
{"pattern": "eval", "package": "CL-MCP-SERVER.EVALUATOR"}
```

Output:
```
Found 1 symbol matching 'eval':

  CL-MCP-SERVER.EVALUATOR::EVALUATE-CODE [FUNCTION]
```

### Type-Filtered Search

Input:
```json
{"pattern": "def", "type": "macro"}
```

Output:
```
Found 15 symbols matching 'def':

  COMMON-LISP::DEFCLASS [MACRO]
  COMMON-LISP::DEFCONSTANT [MACRO]
  COMMON-LISP::DEFGENERIC [MACRO]
  COMMON-LISP::DEFINE-CONDITION [MACRO]
  COMMON-LISP::DEFMACRO [MACRO]
  COMMON-LISP::DEFMETHOD [MACRO]
  COMMON-LISP::DEFPACKAGE [MACRO]
  COMMON-LISP::DEFPARAMETER [MACRO]
  COMMON-LISP::DEFSETF [MACRO]
  COMMON-LISP::DEFSTRUCT [MACRO]
  COMMON-LISP::DEFTYPE [MACRO]
  COMMON-LISP::DEFUN [MACRO]
  COMMON-LISP::DEFVAR [MACRO]
  ...
```

### No Results

Input:
```json
{"pattern": "xyznonexistent"}
```

Output:
```
Found 0 symbols matching 'xyznonexistent':

```

## Error Response

When apropos-search fails:

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

Note: Search failures return descriptive messages but `isError: false` since returning no results is a valid outcome.

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| Package not found | Invalid package name | "Package X not found" |
| Invalid type | Unknown type filter | "Invalid type: X. Valid types: function, macro, variable, class, generic-function" |

### Package Not Found

Input:
```json
{"pattern": "foo", "package": "NONEXISTENT"}
```

Response:
```
Package NONEXISTENT not found
```

## Implementation Notes

- When no package specified, uses `do-external-symbols` on all packages
- When package specified, uses `do-symbols` (includes internal symbols)
- Type classification uses same logic as `describe-symbol`
