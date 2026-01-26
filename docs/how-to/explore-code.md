# How to: Explore and Understand Lisp Code

Learn how to use introspection tools to explore unfamiliar Common Lisp code.

## When to Use This

- You're reading a library you haven't used before
- You found a function name but don't know what it does
- You want to understand how code is organized
- You're debugging and need to trace function calls

## Prerequisites

- CL-MCP-Server running
- Basic familiarity with Common Lisp syntax

## Steps

### 1. Find Functions by Pattern

Use `apropos-search` to discover available functions:

```
User: Please apropos-search for "map"

Response:
Found 12 symbols matching 'map':

  COMMON-LISP::MAP [FUNCTION]
  COMMON-LISP::MAPC [FUNCTION]
  COMMON-LISP::MAPCAR [FUNCTION]
  ...
```

**When to use:**
- Exploring a new library
- "I know it's called something like..."
- Finding all functions of a type

### 2. Get Function Details

Use `describe-symbol` to learn what a function does:

```
User: Please describe-symbol "mapcar" in package "CL"

Response:
COMMON-LISP::MAPCAR [FUNCTION]
  Arglist: (FUNCTION LIST &REST MORE-LISTS)
  Documentation:
    Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
    Return list of FUNCTION return values.
  Source: SYS:SRC;CODE;LIST.LISP:51454
```

**What you learn:**
- Function arguments
- What it returns
- Where it's defined

### 3. Check Who Calls a Function

Use `who-calls` to understand usage:

```
User: Please who-calls "validate-input" in package "MY-APP"

Response:
Functions calling VALIDATE-INPUT:

  MY-APP::PROCESS-REQUEST
  MY-APP::HANDLE-USER-DATA
  MY-APP::API-ENDPOINT
```

**Use cases:**
- Understanding function relationships
- Finding where to add logging
- Planning refactoring

### 4. Validate Code Syntax

Use `validate-syntax` before saving edits:

```
User: Please validate-syntax:
     "(defun calculate-total (items)
        (reduce #'+ items :key #'price)"

Response:
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: 1
Approximate location: line 2
```

**Workflow:**
1. Edit code
2. Validate
3. Fix errors
4. Validate again
5. Save when valid

### 5. Understand Macros

Use `macroexpand-form` to see what macros do:

```
User: Please macroexpand-form "(when (> x 10) (print x))"

Response:
Original:
  (WHEN (> X 10) (PRINT X))

Expanded:
  (IF (> X 10) (PROGN (PRINT X)))
```

**Helps with:**
- Understanding DSL macros
- Debugging macro-related issues
- Learning macro patterns

## Common Workflows

### Exploring a New Library

```
1. apropos-search "libraryname" - Find exported symbols
2. describe-symbol on key functions - Read documentation
3. macroexpand-form on DSL macros - Understand syntax
4. Try simple examples with evaluate-lisp
```

### Understanding Existing Code

```
1. describe-symbol on main function - Understand purpose
2. who-calls to find callers - See usage context
3. Read source (if available) - Study implementation
4. macroexpand-form on complex forms - Clarify behavior
```

### Before Refactoring

```
1. describe-symbol on target function - Document current API
2. who-calls to find all callers - Identify impact
3. Plan changes
4. validate-syntax on new code - Verify correctness
5. Test with callers
```

## Tips

**Start broad, then narrow:**
- Begin with apropos-search
- Use describe-symbol for details
- Check who-calls for relationships

**Verify before executing:**
- Use validate-syntax for edits
- Use compile-form to check types
- Use evaluate-lisp only when ready

**Combine tools:**
- apropos-search finds names
- describe-symbol explains them
- who-calls shows usage
- macroexpand-form clarifies macros

## Troubleshooting

### "Symbol not found"

**Problem:** Package not loaded or wrong package name.

**Solution:**
```
1. Check available packages: (list-all-packages)
2. Load system if needed: quickload "system-name"
3. Try without package parameter (searches all)
```

### "No source location"

**Problem:** Code compiled without source tracking.

**Solution:** Source locations may not be available for:
- Built-in functions
- Code compiled with optimization
- Quicklisp-installed systems

### "Too many results"

**Problem:** apropos-search pattern too broad.

**Solution:**
```
1. Use more specific pattern: "map" → "mapcar"
2. Add type filter: type="function"
3. Add package filter: package="CL"
```

## Related

- [Introspection Tools Reference](../reference/introspection-tools.md) - Complete tool documentation
- [CLOS Tools](../reference/clos-tools.md) - For inspecting classes
- [Tutorial: Exploring CLOS](../tutorials/exploring-clos.md) - Hands-on CLOS exploration
