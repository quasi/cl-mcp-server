# Introspection Tools Reference

<!-- Generated from: canon/features/introspection/contracts/*.md -->

Tools for exploring, understanding, and analyzing Common Lisp code without executing it.

## Overview

The CL-MCP-Server provides a suite of introspection tools that allow you to:
- Look up symbol information (functions, variables, classes)
- Search for symbols by name patterns
- Validate code syntax before evaluation
- Expand macros to understand their transformations
- Track code relationships (who calls what)

These tools are read-only and safe—they never modify code or execute side effects.

## Available Tools

| Tool | Purpose | Use When |
|------|---------|----------|
| [describe-symbol](#describe-symbol) | Get comprehensive information about a symbol | You want to know what a function does, its arguments, or its source |
| [apropos-search](#apropos-search) | Find symbols matching a pattern | You're looking for available functions or exploring a package |
| [validate-syntax](#validate-syntax) | Check code syntax without executing | You want to verify code before saving or find paren mismatches |
| [who-calls](#who-calls) | Find all callers of a function | You want to understand function usage or plan refactoring |
| [who-references](#who-references) | Find all references to a variable | You want to track where a variable is read |
| [macroexpand-form](#macroexpand-form) | Expand macros to see their transformation | You want to understand what a macro does |

---

## describe-symbol

Get comprehensive information about a Lisp symbol including type, value, documentation, arglist, and source location.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| name | string | Yes | Symbol name to describe |
| package | string | No | Package name (defaults to CL-USER) |

### Output

Shows all available information about the symbol:
- Symbol type (function, macro, variable, class, etc.)
- Function/macro argument list
- Current value (for variables)
- Documentation string (if available)
- Source file location (if available)

### Examples

**Looking up a built-in function:**

```
User: Please use describe-symbol on "mapcar" in package "CL"

Response:
COMMON-LISP::MAPCAR [FUNCTION]
  Arglist: (FUNCTION LIST &REST MORE-LISTS)
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return list of FUNCTION return values.
  Source: SYS:SRC;CODE;LIST.LISP:51454
```

**Checking a variable:**

```
User: Please use describe-symbol on "*print-base*" in package "CL"

Response:
COMMON-LISP::*PRINT-BASE* [VARIABLE]
  Value: 10
  Documentation:
    The output base for rationals.
```

**Symbol not found:**

```
User: Please use describe-symbol on "nonexistent-symbol"

Response:
Symbol NONEXISTENT-SYMBOL not found in package CL-USER (status: NIL)
```

### Notes

- Uses SBCL's `sb-introspect` for detailed information
- Source locations for built-in symbols may show SBCL internal paths (`SYS:...`)
- Variable values are printed with safety limits (*print-length* = 20, *print-level* = 3)

---

## apropos-search

Search for symbols matching a pattern with optional type and package filtering.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| pattern | string | Yes | Search pattern (case-insensitive substring) |
| package | string | No | Limit search to this package |
| type | string | No | Filter by type: `function`, `macro`, `variable`, `class`, `generic-function` |

### Output

Returns a list of matching symbols with their types and packages:

```
Found N symbols matching 'PATTERN':

  PACKAGE-1::SYMBOL-A [FUNCTION]
  PACKAGE-2::SYMBOL-B [MACRO]
  PACKAGE-3::SYMBOL-C [VARIABLE]
```

Results are sorted alphabetically by symbol name.

### Examples

**Find all map functions:**

```
User: Please use apropos-search to find symbols with "map"

Response:
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

**Search within a specific package:**

```
User: Please use apropos-search with pattern "eval" in package "CL-MCP-SERVER.EVALUATOR"

Response:
Found 1 symbol matching 'eval':

  CL-MCP-SERVER.EVALUATOR::EVALUATE-CODE [FUNCTION]
```

**Filter by type:**

```
User: Please use apropos-search for "def" with type "macro"

Response:
Found 15 symbols matching 'def':

  COMMON-LISP::DEFCLASS [MACRO]
  COMMON-LISP::DEFUN [MACRO]
  COMMON-LISP::DEFVAR [MACRO]
  COMMON-LISP::DEFMACRO [MACRO]
  ...
```

### Search Scope

| Scenario | Scope |
|----------|-------|
| No package specified | External symbols of all packages |
| Package specified | All symbols (including internal) in that package |

### Notes

- Pattern matching is case-insensitive
- Broad patterns (like "a") may return many results
- Combine with type filter to narrow results

---

## validate-syntax

Check if Common Lisp code is syntactically valid without evaluating it. Detects unbalanced parentheses, reader errors, and other syntax issues.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| code | string | Yes | Common Lisp code to validate |

### Output

**Valid code:**
```
✓ Syntax valid: N top-level form(s)
```

**Invalid code (unclosed parentheses):**
```
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: N
Approximate location: line M
```

**Invalid code (extra close paren):**
```
✗ Syntax invalid

Error: unmatched close parenthesis
```

### Examples

**Valid code:**

```
User: Please validate-syntax: "(defun square (x) (* x x))"

Response:
✓ Syntax valid: 1 top-level form(s)
```

**Missing close parenthesis:**

```
User: Please validate-syntax: "(defun square (x) (* x x)"

Response:
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: 1
Approximate location: line 1
```

**Extra close parenthesis:**

```
User: Please validate-syntax: "(+ 1 2)))"

Response:
✗ Syntax invalid

Error: unmatched close parenthesis
```

### Use Cases

1. **Pre-save validation**: Check code syntax before writing to files
2. **Edit verification**: Validate edits before committing changes
3. **Incremental development**: Verify partial code while building
4. **Error prevention**: Catch paren mismatches early

### Recommended Workflow

```
1. Read existing file
2. Make edits (compute new content)
3. Validate with validate-syntax
4. Only save if validation passes
5. If invalid, fix and repeat from step 3
```

### What It Detects

| Error Type | Detected | Information Provided |
|------------|----------|---------------------|
| Missing close paren | ✓ | Count of unclosed, line hint |
| Extra close paren | ✓ | Reader error message |
| Unterminated string | ✓ | Reader error message |
| Invalid characters | ✓ | Reader error message |

### Notes

- Does not execute any code
- Does not require package context
- Line hints are approximate (character-based)
- Handles strings and comments correctly

---

## who-calls

Find all functions that call the specified function. Uses SBCL's cross-reference database.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| name | string | Yes | Function name to find callers of |
| package | string | No | Package name (defaults to CL-USER) |

### Output

Lists all functions that call the target function:

```
Functions calling FUNCTION-NAME:

  PACKAGE::CALLER-1
  PACKAGE::CALLER-2
  PACKAGE::CALLER-3
```

If no callers are found:

```
No known callers of FUNCTION-NAME
```

### Example

```
User: Please find who calls "format-error" in package "CL-MCP-SERVER"

Response:
Functions calling FORMAT-ERROR:

  CL-MCP-SERVER.EVALUATOR::HANDLE-EVALUATION-ERROR
  CL-MCP-SERVER.TOOLS::CREATE-ERROR-RESPONSE
```

### Notes

- Requires SBCL with source tracking enabled
- Only shows compiled function calls (not interpreted code)
- May not show all callers if code was compiled without cross-reference data

---

## who-references

Find all code that references (reads) a variable. Uses SBCL's cross-reference database.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| name | string | Yes | Variable name to find references to |
| package | string | No | Package name (defaults to CL-USER) |

### Output

Lists all locations that reference the variable:

```
Code referencing VARIABLE-NAME:

  PACKAGE::FUNCTION-1
  PACKAGE::FUNCTION-2
```

If no references found:

```
No known references to VARIABLE-NAME
```

### Notes

- Tracks variable reads, not assignments
- Uses SBCL's cross-reference system
- Only available for compiled code with xref data

---

## macroexpand-form

Expand macros in a Lisp form to understand their transformation.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| form | string | Yes | Lisp form to expand (as a string) |
| full | boolean | No | If true, fully expand all macros recursively (default: false - one step) |
| package | string | No | Package context for reading the form (defaults to CL-USER) |

### Output

Shows the expanded form:

```
Original:
  <input form>

Expanded:
  <expanded form>
```

### Examples

**One-step expansion:**

```
User: Please macroexpand-form "(when (> x 10) (print x))"

Response:
Original:
  (WHEN (> X 10) (PRINT X))

Expanded:
  (IF (> X 10) (PROGN (PRINT X)))
```

**Full expansion:**

```
User: Please macroexpand-form "(when (> x 10) (print x))" with full=true

Response:
Original:
  (WHEN (> X 10) (PRINT X))

Expanded:
  (IF (> X 10) (PROGN (PRINT X)))

(No further expansion - PROGN and IF are special forms)
```

### Notes

- One-step expansion shows the immediate transformation
- Full expansion recursively expands until reaching special forms or functions
- Does not execute code, only expands macro calls
- Useful for understanding complex macro transformations

---

## Common Workflows

### Exploring an Unknown Codebase

```
1. apropos-search - Find interesting symbols
2. describe-symbol - Learn what each symbol does
3. who-calls - Understand usage patterns
4. macroexpand-form - Understand macros
```

### Refactoring a Function

```
1. who-calls - Find all callers
2. describe-symbol - Verify current signature
3. Make changes
4. validate-syntax - Verify syntax
5. Test with callers
```

### Understanding a Library

```
1. apropos-search - List package exports
2. describe-symbol - Read documentation
3. macroexpand-form - Understand DSL macros
```

---

## See Also

- [evaluate-lisp Tool](evaluate-lisp.md) - Execute Common Lisp code
- [compile-form Tool](enhanced-evaluation.md#compile-form) - Compile code without executing
- [How to: Explore Code](../how-to/explore-code.md) - Practical introspection guide
