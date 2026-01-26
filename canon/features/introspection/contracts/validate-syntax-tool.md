---
type: contract
name: validate-syntax-tool
version: 0.1.0
phase: A
---

# Validate-Syntax Tool Contract

Validates Common Lisp code syntax without evaluation. Detects unbalanced parentheses, reader errors, and other syntax issues before code is saved or executed.

## Tool Definition

```json
{
  "name": "validate-syntax",
  "description": "Check if Common Lisp code is syntactically valid without evaluating it. Detects unbalanced parentheses, reader errors, and other syntax issues. Use this to verify code before saving or executing.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp code to validate"
      }
    }
  }
}
```

## Output Format

### Valid Code

```
✓ Syntax valid: N top-level form(s)
```

### Invalid Code - Unclosed Parentheses

```
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: N
Approximate location: line M
```

### Invalid Code - Extra Close Parenthesis

```
✗ Syntax invalid

Error: unmatched close parenthesis
  Stream: ...
```

### Invalid Code - Other Reader Errors

```
✗ Syntax invalid

Error: <reader error description>
```

## Implementation

Uses the Lisp reader to parse code without evaluation:

```lisp
(with-input-from-string (stream code-string)
  (loop for form = (read stream nil :eof)
        until (eq form :eof)
        count t))
```

When parsing fails, analyzes the code to estimate:
- Number of unclosed parentheses
- Approximate line number of the problem

## Use Cases

1. **Pre-save Validation**: Check code syntax before writing to files
2. **Agent Workflow**: Validate edits before committing changes
3. **Incremental Development**: Verify partial code while building
4. **Error Prevention**: Catch paren mismatches early

## Workflow Integration

This tool is designed to support a validate-before-save workflow:

```
1. Read existing file
2. Make edits (compute new content)
3. Validate new content with validate-syntax
4. Only save if validation passes
5. If invalid, fix and repeat from step 3
```

## Error Detection Capabilities

| Error Type | Detection | Information Provided |
|------------|-----------|---------------------|
| Missing close paren | Yes | Count of unclosed, line hint |
| Extra close paren | Yes | Reader error message |
| Unterminated string | Yes | Reader error message |
| Invalid syntax | Yes | Reader error message |
| Invalid characters | Yes | Reader error message |

## Comparison with Other Tools

| Tool | Purpose | Executes Code? |
|------|---------|----------------|
| validate-syntax | Syntax check only | No |
| compile-form | Type/warning check | No (but compiles) |
| evaluate-lisp | Full execution | Yes |

## Notes

- Does not evaluate any code
- Does not require package context (uses reader only)
- Line hints are approximate (based on character counting)
- Handles strings and comments correctly when counting parens
