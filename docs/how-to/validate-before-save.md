# How to: Validate Lisp Code Before Saving

When editing Lisp files, parenthesis mismatches are easy to introduce and hard to spot. The `validate-syntax` tool helps catch these errors before they're saved to files.

## The Problem

Unlike most languages, Lisp's structure is entirely determined by parentheses. A single missing or extra paren can:
- Break the entire file
- Cause cryptic read errors
- Be difficult to locate in long functions

Tools like [Paredit](https://www.emacswiki.org/emacs/ParEdit) prevent these errors by maintaining balance as an invariant. When editing without structural editing support (like AI agents do), validation becomes essential.

## The Solution: Validate Before Save

Use the `validate-syntax` tool to check code before writing it to files:

```
1. Read existing file content
2. Compute new content with edits
3. Call validate-syntax with new content
4. If valid: save the file
5. If invalid: fix errors and repeat from step 3
```

## Using validate-syntax

### Tool Definition

```json
{
  "name": "validate-syntax",
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

### Valid Code Response

```
✓ Syntax valid: 3 top-level forms
```

### Invalid Code Response

```
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: 1
Approximate location: line 42
```

## Example Workflow

### Step 1: Read Current File

```lisp
;; Current content of utils.lisp
(defun helper (x)
  (+ x 1))

(defun main (args)
  (mapcar #'helper args))
```

### Step 2: Plan Edit

You want to add error handling to `main`:

```lisp
(defun main (args)
  (handler-case
      (mapcar #'helper args)
    (error (c)
      (format t "Error: ~A~%" c)
      nil)))
```

### Step 3: Validate Before Save

Call `validate-syntax` with the complete new file content:

```json
{
  "code": "(defun helper (x)\n  (+ x 1))\n\n(defun main (args)\n  (handler-case\n      (mapcar #'helper args)\n    (error (c)\n      (format t \"Error: ~A~%\" c)\n      nil)))"
}
```

### Step 4: Check Result

If valid:
```
✓ Syntax valid: 2 top-level forms
```

Proceed to save.

If invalid (e.g., you forgot a closing paren):
```
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: 1
Approximate location: line 9
```

Fix and re-validate before saving.

## Best Practices

### 1. Validate Complete Files

Always validate the entire file content, not just the edited section. This catches imbalances that span multiple forms.

### 2. Use With compile-form for Extra Safety

For additional checking, combine with `compile-form`:

1. `validate-syntax` - catches syntax errors (fast)
2. `compile-form` - catches type errors and warnings (thorough)

### 3. Make Targeted Edits

Reduce error risk by editing specific forms rather than rewriting large sections:

**Instead of**: Replacing an entire 100-line file
**Prefer**: Editing the specific function that needs changes

### 4. Understand Form Boundaries

Before editing, understand where forms start and end. The error location in validation output helps identify form boundaries.

## Comparison with Other Tools

| Tool | Purpose | Speed | Depth |
|------|---------|-------|-------|
| validate-syntax | Syntax only | Fast | Shallow |
| compile-form | Type checking | Medium | Deep |
| evaluate-lisp | Full execution | Slow | Full |

Use `validate-syntax` for quick checks during editing, `compile-form` for pre-commit verification.

## Troubleshooting

### "Unclosed parentheses: N"

You have N more open parens than close parens. Check:
- Function definitions that span multiple lines
- `let` bindings with multiple variables
- Nested `cond`/`case` clauses

### "unmatched close parenthesis"

You have an extra closing paren. Often caused by:
- Copy-paste errors
- Accidental duplication
- Removing code without removing its closing paren

### Line numbers don't match

Line hints are approximate. The actual error may be earlier in the file where the imbalance began.

## See Also

- [validate-syntax-tool contract](../../canon/features/introspection/contracts/validate-syntax-tool.md)
- [syntax-validation scenarios](../../canon/features/introspection/scenarios/syntax-validation.md)
