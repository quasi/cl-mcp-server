---
type: property
name: condition-type-preservation
version: 1.0.0
feature: error-handling
covers:
  - contracts/condition-report
relates_to:
  - core/invariants#INV-006
---

# Condition Type Preservation Property

## Statement

**For all** Lisp conditions that occur during evaluation,
**the error response** MUST include the condition type (class name),
**not just** the condition message text.

## Formal Expression

```
∀ condition ∈ Conditions :
  let response = handle_condition(condition) in
    response.text contains (type-of condition) ∧
    response.text contains (princ-to-string condition)

where handle_condition captures both:
  - condition.type → string representation of the class
  - condition.message → formatted description
```

## Informal Explanation

When user code signals an error, the error response must preserve and report the condition type (e.g., `UNDEFINED-FUNCTION`, `TYPE-ERROR`, `DIVISION-BY-ZERO`) alongside the human-readable message.

This applies to:
- Built-in Common Lisp conditions
- Implementation-specific conditions (e.g., `SB-KERNEL:INDEX-TOO-LARGE-ERROR`)
- User-defined conditions

The format specified in the condition-report contract is:
```
[ERROR] {CONDITION-TYPE}
{message}

[Backtrace]
...
```

## Rationale

Condition types carry semantic information that message text alone cannot convey:
- `UNDEFINED-FUNCTION` tells Claude the symbol is not bound to a function
- `TYPE-ERROR` indicates a type contract violation
- `DIVISION-BY-ZERO` is a specific arithmetic error

Claude can use this information to:
- Provide more accurate diagnostics
- Suggest appropriate fixes
- Distinguish between error categories (syntax vs. runtime vs. type errors)

Without type information, all errors look like generic failures, losing diagnostic precision.

## Counterexample Shape

If this property is violated, you might see:

**Bad**: Error response with message only
```json
{
  "content": [{"type": "text", "text": "The function FOO is undefined."}],
  "isError": true
}
```

**Good**: Error response with type and message
```json
{
  "content": [{"type": "text", "text": "[ERROR] UNDEFINED-FUNCTION\nThe function FOO is undefined.\n\n[Backtrace]\n..."}],
  "isError": true
}
```

Other violations:
- Generic error type like "ERROR" instead of specific type
- Condition type not included in response text
- Type information lost during condition handling
- Implementation details obscuring the condition type

## Verification Approach

**Generator**: Generate code that signals known condition types

**Test Matrix**:
```lisp
(defvar *test-conditions*
  '(;; Undefined symbols
    ("(nonexistent-fn 1 2)" . "UNDEFINED-FUNCTION")
    ("(+ x 1)" . "UNBOUND-VARIABLE")

    ;; Type errors
    ("(+ 1 \"hello\")" . "TYPE-ERROR")
    ("(car 42)" . "TYPE-ERROR")

    ;; Arithmetic errors
    ("(/ 1 0)" . "DIVISION-BY-ZERO")

    ;; Array errors
    ("(aref #(1 2 3) 10)" . "INDEX-TOO-LARGE-ERROR")

    ;; Reader errors
    ("(+ 1 2" . "END-OF-FILE")
    ("(+ 1 #\\(" . "READER-ERROR")))
```

**Assertion**:
```lisp
(defun verify-condition-type-preservation (code expected-type)
  (let ((response (evaluate-code code)))
    ;; Response should be an error
    (assert (error-response-p response))

    ;; Response text should contain condition type
    (let ((text (response-text response)))
      (assert (search (string-upcase expected-type) text)
              nil
              "Expected condition type ~A not found in response: ~A"
              expected-type text)

      ;; Should follow [ERROR] TYPE format
      (assert (search "[ERROR]" text))
      (assert (search expected-type text :start2 (search "[ERROR]" text))))))
```

**Property Test**:
- For each known condition type, trigger it
- Verify the error response includes the type name
- Verify the type appears after "[ERROR]" marker
- Verify the format matches the contract

**Edge Cases**:
- Implementation-specific conditions (e.g., SBCL-specific types)
- Nested conditions (condition signaled during condition handling)
- User-defined condition classes
- Conditions with very long type names
- Package-qualified condition names (e.g., `CL:DIVISION-BY-ZERO`)

**Shrinking**: Find minimal code that signals a condition whose type is not preserved
