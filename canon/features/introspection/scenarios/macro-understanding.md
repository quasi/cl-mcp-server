---
type: scenario
name: macro-understanding
feature: introspection
version: 0.1.0
---

# Macro Understanding Scenarios

Scenarios for using macroexpand-form to understand macro transformations.

## Single-Step Expansion Scenarios

### Scenario: Expand DEFUN

**When** I call macroexpand-form with form="(defun foo (x) (1+ x))"
**Then** the response shows SBCL's DEFUN expansion:
  - EVAL-WHEN forms for compiler
  - SB-IMPL::%DEFUN call
  - Named-lambda wrapping

### Scenario: Expand PUSH

**When** I call macroexpand-form with form="(push item list)"
**Then** the response shows the SETF expansion:
  - May show LET bindings for safe evaluation
  - CONS call to prepend item
  - SETQ or SETF to update the place

### Scenario: Expand WITH-OPEN-FILE

**When** I call macroexpand-form with form="(with-open-file (s \"test.txt\") (read s))"
**Then** the response shows:
  - LET binding for stream variable
  - OPEN call
  - UNWIND-PROTECT for cleanup
  - CLOSE in cleanup form

### Scenario: Expand LOOP

**When** I call macroexpand-form with form="(loop for i from 1 to 10 collect (* i i))"
**Then** the response shows LOOP's expansion:
  - BLOCK for return
  - LET bindings for iteration variables
  - TAGBODY with GO for iteration
  - Accumulator management for COLLECT

## Full Expansion Scenarios

### Scenario: Full Expansion of Nested Macros

**When** I call macroexpand-form with form="(defmacro my-when (test &body body) `(if ,test (progn ,@body)))" and full=true
**Then** the response shows fully expanded code:
  - All macro calls recursively expanded
  - Only special forms and function calls remain

### Scenario: Full vs Single-Step Difference

**Given** a form with nested macros
**When** I call macroexpand-form with full=false
**Then** only the outermost macro is expanded
**When** I call macroexpand-form with full=true
**Then** all macros in the result are also expanded

## Edge Cases

### Scenario: Non-Macro Form

**When** I call macroexpand-form with form="(+ 1 2)"
**Then** the response shows:
  - The original form unchanged
  - Note that "(Form is not a macro call)"

### Scenario: Special Form

**When** I call macroexpand-form with form="(if t 1 2)"
**Then** the response shows:
  - The original form unchanged (IF is a special form, not a macro)
  - Note that the form is not a macro call

### Scenario: Invalid Form Syntax

**When** I call macroexpand-form with form="(defun"
**Then** the response indicates a read error:
  - "Error reading form: end of file..."

### Scenario: Form with Undefined Macro

**When** I call macroexpand-form with form="(undefined-macro arg)"
**Then** the response shows:
  - The form unchanged (it's treated as a function call)
  - Note that it's not a macro call

## Practical Use Cases

### Scenario: Understanding Custom Macro Behavior

**Given** I have defined a custom macro `with-my-resource`
**When** I call macroexpand-form with that macro's usage
**Then** I can see exactly what code my macro generates

### Scenario: Debugging Macro Issues

**Given** a macro is not behaving as expected
**When** I expand the problematic form
**Then** I can inspect the generated code for issues:
  - Variable capture problems
  - Incorrect code generation
  - Missing or extra evaluations

### Scenario: Learning SBCL Internals

**When** I expand standard macros like DEFCLASS or DEFGENERIC
**Then** I can learn how SBCL implements these forms:
  - Internal functions used
  - Optimizations applied
  - Metaclass protocols
