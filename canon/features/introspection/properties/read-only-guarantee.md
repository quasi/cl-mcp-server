---
type: property
name: read-only-guarantee
version: 0.1.0
feature: introspection
covers:
  - contracts/describe-symbol-tool
  - contracts/apropos-search-tool
  - contracts/who-calls-tool
  - contracts/who-references-tool
  - contracts/macroexpand-form-tool
  - contracts/validate-syntax-tool
---

# Read-Only Guarantee Property

## Statement

**For all** introspection tool invocations,
**the tools** MUST NOT modify any session state, symbol definitions, variable bindings, or package structures.

## Formal Expression

```
∀ tool ∈ IntrospectionTools, ∀ state₀ ∈ SessionState :
  let state₁ = execute(tool, state₀)
  then observable_state(state₁) = observable_state(state₀)

where:
  IntrospectionTools = {
    describe-symbol,
    apropos-search,
    who-calls,
    who-references,
    macroexpand-form,
    validate-syntax
  }

  observable_state(s) = {
    symbols(s),
    bindings(s),
    packages(s),
    definitions(s),
    classes(s)
  }
```

## Informal Explanation

All introspection tools are purely observational. They must:

1. **Not modify symbols**: No `setf`, `defun`, `defvar`, etc.
2. **Not change bindings**: Special variables retain their values
3. **Not alter packages**: No new symbols, no package modifications
4. **Not define code**: No side effects from reading or expanding
5. **Not evaluate user code**: Only parse and inspect

This property ensures Claude can safely explore the Lisp environment without risk of accidentally modifying the user's code or state.

## Rationale

Introspection tools are meant for discovery and understanding, not modification. Users expect these tools to be safe to run at any time without side effects. Violating this would break the "look but don't touch" contract.

Key safety implications:
- Symbol lookup uses `find-symbol`, not `intern` (no symbol creation)
- Macro expansion doesn't evaluate forms
- Syntax validation only parses, doesn't execute
- Cross-reference queries only read SBCL's xref database

## Counterexample Shape

If this property is violated, you might see:

**State Modification Examples**:
- `describe-symbol` interns a symbol if not found
- `apropos-search` creates new package entries
- `macroexpand-form` evaluates the expanded code
- `validate-syntax` has side effects from the reader

**Observable Changes**:
```lisp
;; Before tool call
(fboundp 'my-new-function) → NIL

;; After describe-symbol call
(fboundp 'my-new-function) → T  ; VIOLATION!

;; Before tool call
(package-external-symbols "MY-PKG") → (list of N symbols)

;; After apropos-search
(package-external-symbols "MY-PKG") → (list of N+1 symbols)  ; VIOLATION!
```

## Verification Approach

**Generator**: Generate random introspection tool calls with valid inputs

**Assertion**:
```lisp
(defun verify-read-only (tool-name params)
  ;; Capture state before
  (let ((symbols-before (collect-all-symbols))
        (packages-before (list-all-packages))
        (bindings-before (capture-special-vars)))

    ;; Execute introspection tool
    (call-tool tool-name params)

    ;; Verify state unchanged
    (and (equal symbols-before (collect-all-symbols))
         (equal packages-before (list-all-packages))
         (equal bindings-before (capture-special-vars)))))
```

**Property Test Strategy**:

1. **Symbol Creation Test**:
   - Enumerate all symbols before tool call
   - Run introspection tool
   - Verify no new symbols exist
   - Especially test with nonexistent symbol names

2. **Variable Binding Test**:
   - Set sentinel special variables to known values
   - Run introspection tools
   - Verify values unchanged

3. **Package Modification Test**:
   - Count symbols per package before
   - Run introspection tools
   - Verify counts unchanged

4. **Definition Test**:
   - Check `fboundp` for random symbols before
   - Run describe-symbol on them
   - Verify `fboundp` unchanged

5. **Macro Expansion Test**:
   - Expand a macro with side effects: `(defparameter *test* 42)`
   - Verify `*test*` is unbound after expansion
   - Verify no code was evaluated

**Edge Cases**:
- Querying symbols that don't exist (must not intern them)
- Expanding macros with evaluation side effects (must not execute)
- Reading malformed code in validate-syntax (must not leak state)
- Searching packages with side-effect-laden symbol-name methods

**Implementation Notes**:
```lisp
;; Safe symbol lookup (does not intern)
(find-symbol name package)  ; NOT (intern name package)

;; Safe macro expansion (does not evaluate)
(macroexpand-1 form)  ; NOT (eval form)

;; Safe code reading (does not execute)
(read-from-string code)  ; NOT (eval (read-from-string code))
```

**Shrinking**: Find minimal tool invocation that causes state change
