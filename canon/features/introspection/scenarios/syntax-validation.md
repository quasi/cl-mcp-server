---
type: scenario
name: syntax-validation
feature: introspection
---

# Syntax Validation Scenarios

Testing the validate-syntax tool for detecting syntax errors before evaluation.

## Scenario: Valid Single Form

**Given** valid Lisp code with one form
**When** validate-syntax is called
**Then** result shows valid with 1 form

```json
{"code": "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))"}
```

**Expected Output**:
```
✓ Syntax valid: 1 top-level form
```

## Scenario: Valid Multiple Forms

**Given** valid Lisp code with multiple forms
**When** validate-syntax is called
**Then** result shows valid with correct form count

```json
{"code": "(defvar *x* 1)\n(defvar *y* 2)\n(defun add () (+ *x* *y*))"}
```

**Expected Output**:
```
✓ Syntax valid: 3 top-level forms
```

## Scenario: Missing Close Parenthesis

**Given** code with unclosed parenthesis
**When** validate-syntax is called
**Then** result shows invalid with unclosed count

```json
{"code": "(defun foo (x)\n  (let ((y 1))\n    (+ x y)"}
```

**Expected Output**:
```
✗ Syntax invalid

Error: Unexpected end of input - unclosed parenthesis
Unclosed parentheses: 1
Approximate location: line 3
```

## Scenario: Extra Close Parenthesis

**Given** code with extra closing parenthesis
**When** validate-syntax is called
**Then** result shows reader error

```json
{"code": "(defun foo (x) (+ x 1)))"}
```

**Expected Output**:
```
✗ Syntax invalid

Error: unmatched close parenthesis
  Stream: ...
```

## Scenario: Deeply Nested Valid Code

**Given** deeply nested but balanced code
**When** validate-syntax is called
**Then** result shows valid

```json
{"code": "(a (b (c (d (e (f (g 1)))))))"}
```

**Expected Output**:
```
✓ Syntax valid: 1 top-level form
```

## Scenario: Parentheses Inside Strings

**Given** code with parentheses inside string literals
**When** validate-syntax is called
**Then** string parens are ignored correctly

```json
{"code": "(defun foo () \"This has ( and ) in it\")"}
```

**Expected Output**:
```
✓ Syntax valid: 1 top-level form
```

## Scenario: Empty Input

**Given** empty code string
**When** validate-syntax is called
**Then** result shows valid with 0 forms

```json
{"code": ""}
```

**Expected Output**:
```
✓ Syntax valid: 0 top-level forms
```

## Scenario: Whitespace Only

**Given** code with only whitespace
**When** validate-syntax is called
**Then** result shows valid with 0 forms

```json
{"code": "   \n\n   "}
```

**Expected Output**:
```
✓ Syntax valid: 0 top-level forms
```

## Scenario: Comments Only

**Given** code with only comments
**When** validate-syntax is called
**Then** result shows valid with 0 forms

```json
{"code": "; This is a comment\n;; Another comment"}
```

**Expected Output**:
```
✓ Syntax valid: 0 top-level forms
```

## Workflow Scenario: Validate Before Save

**Context**: Agent is editing a Lisp file and wants to verify syntax before saving.

1. Agent reads current file content
2. Agent computes new content with edits
3. Agent calls validate-syntax with new content
4. If valid: proceed to save
5. If invalid: fix errors and repeat from step 3

This workflow prevents saving syntactically broken code.
