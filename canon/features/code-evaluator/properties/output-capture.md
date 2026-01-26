---
type: property
name: output-capture
version: 1.0.0
feature: code-evaluator
covers:
  - contracts/evaluate-lisp-tool
relates_to:
  - core/invariants#INV-004
---

# Output Capture Property

## Statement

**For all** code evaluations that produce output,
**the server** MUST capture and correctly separate:
- Standard output (`*standard-output*`)
- Error output (`*error-output*`)
- Warnings (via condition handlers)
- Return values (including multiple values)

AND each output type MUST be distinguishable in the response.

## Formal Expression

```
∀ code ∈ Evaluations :
  let result = evaluate(code) in
    ∃ stdout, stderr, warnings, values :
      result = {stdout, stderr, warnings, values} ∧
      distinguishable(stdout, stderr, warnings, values)

where distinguishable(a, b, c, d) ≡
  can_separate(a) ∧ can_separate(b) ∧
  can_separate(c) ∧ can_separate(d)
```

## Informal Explanation

When user code executes, multiple output streams may be active:
- `(format t "hello")` → stdout
- `(format *error-output* "warning")` → stderr
- `(warn "something")` → warnings section
- `(+ 1 2)` → return value 3

The evaluator must:
1. **Capture all output**: Nothing is lost or goes to server console
2. **Separate by type**: Each output type has its own section in response
3. **Preserve order**: Output appears in execution order within each stream
4. **Handle multiple values**: Return values like `(floor 17 5)` → `3, 2`

Response format:
```
[stdout]
{stdout content}

[stderr]
{stderr content}

[warnings]
WARNING: {warning message}

=> {value1}
=> {value2}
```

Empty sections are omitted from the response.

## Rationale

Claude needs to understand what output means:
- Return values are computation results
- Stdout is informational output
- Stderr is warnings/diagnostics
- Warnings are condition signals

Without separation, Claude cannot distinguish between:
- `(format t "42") NIL` (prints "42", returns NIL)
- `42` (returns 42)

This property directly implements INV-004 (Output Stream Separation).

## Counterexample Shape

If this property is violated, you might see:
- Stdout and return value mixed together indistinguishably
- Warnings printed to stdout instead of separate section
- Error output lost or merged with stdout
- Multiple return values appear as single value
- Output sections appear in wrong order
- Empty sections included when no output occurred

## Verification Approach

**Property Test - Stream Separation**:

```lisp
(defun test-stream-separation ()
  ;; Code that uses all output types
  (let* ((code "(format t \"out\") (format *error-output* \"err\") (warn \"wrn\") :result")
         (response (evaluate code)))
    (assert (has-section response "[stdout]" "out"))
    (assert (has-section response "[stderr]" "err"))
    (assert (has-section response "[warnings]" "WARNING: wrn"))
    (assert (has-section response "=>" ":RESULT"))))
```

**Property Test - Multiple Values**:

```lisp
(defun test-multiple-values ()
  (let ((response (evaluate "(floor 17 5)")))
    (assert (string-contains response "=> 3"))
    (assert (string-contains response "=> 2"))
    (assert (= 2 (count "=>" response)))))
```

**Property Test - Empty Section Omission**:

```lisp
(defun test-empty-sections-omitted ()
  (let ((response (evaluate "(+ 1 2)")))  ; No output, just value
    (assert (not (string-contains response "[stdout]")))
    (assert (not (string-contains response "[stderr]")))
    (assert (not (string-contains response "[warnings]")))
    (assert (string-contains response "=> 3"))))
```

**Property Test - Output Ordering**:

```lisp
(defun test-output-ordering ()
  (let ((response (evaluate "(print 1) (print 2) (print 3)")))
    (assert (< (position "1" response)
               (position "2" response)))
    (assert (< (position "2" response)
               (position "3" response)))))
```

**Generator**: Generate random code combining:
- Format statements to stdout/stderr
- Warn calls
- Expressions returning values
- Expressions returning multiple values

**Assertion**:
- All output types present in response
- Each type in correct section with correct marker
- No output lost or misattributed
- Empty sections omitted

**Shrinking**: Find minimal code that causes output misattribution

## Related

- [evaluate-lisp-tool](../contracts/evaluate-lisp-tool.md) - Defines output format
- [output-capture scenario](../scenarios/output-capture.md) - Test cases
