---
type: property
name: evaluation-isolation
version: 1.0.0
feature: code-evaluator
covers:
  - contracts/evaluate-lisp-tool
relates_to:
  - core/invariants#INV-003
---

# Evaluation Isolation Property

## Statement

**For all** code evaluations within a session,
**if** the code defines or modifies state (functions, variables, classes),
**then** those definitions MUST persist within the session
AND **must not** affect other sessions or the global server state.

## Formal Expression

```
∀ session ∈ Sessions, ∀ code ∈ Evaluations :
  let result₁ = evaluate(session, "(defun f (x) (* x 2))") in
  let result₂ = evaluate(session, "(f 5)") in
    result₂.value = 10 ∧
    ∀ session' ∈ Sessions : session' ≠ session ⟹
      evaluate(session', "(fboundp 'f)") = NIL
```

## Informal Explanation

Code evaluation provides session isolation through persistent session state:

1. **Within-session persistence**: Definitions remain available for subsequent evaluations in the same session
2. **Cross-session isolation**: Definitions in one session do not leak into other sessions
3. **Server state protection**: User code cannot corrupt the MCP server's internal state

The evaluator creates an isolated environment where user code can safely execute without affecting the server infrastructure.

## Rationale

Session isolation is critical for:
- **REPL experience**: Users build up state incrementally through multiple evaluations
- **Multi-client support**: Multiple Claude instances can connect without interfering
- **Server stability**: User code bugs cannot break the MCP protocol handling

This property relates to INV-003 (Session State Persistence) by ensuring definitions persist within sessions while maintaining isolation between sessions.

## Counterexample Shape

If this property is violated, you might see:
- Function defined in one evaluation is unavailable in next evaluation (persistence failure)
- Function defined in Session A appears in Session B (isolation failure)
- User code can redefine MCP server functions like `handle-initialize`
- User code can access or modify server configuration variables
- Evaluation affects the behavior of subsequent MCP protocol handling

## Verification Approach

**Property Test - Within-Session Persistence**:

```lisp
(defun test-within-session-persistence ()
  (let ((session (make-test-session)))
    ;; Define function
    (let ((r1 (evaluate session "(defun square (x) (* x x))")))
      (assert (result-success-p r1)))

    ;; Use function in next evaluation
    (let ((r2 (evaluate session "(square 7)")))
      (assert (result-success-p r2))
      (assert (equal "49" (result-value r2))))))
```

**Property Test - Cross-Session Isolation**:

```lisp
(defun test-cross-session-isolation ()
  (let ((session-a (make-test-session))
        (session-b (make-test-session)))
    ;; Define in session A
    (evaluate session-a "(defvar *x* 42)")

    ;; Should not exist in session B
    (let ((result (evaluate session-b "(boundp '*x*)")))
      (assert (equal "NIL" (result-value result))))))
```

**Property Test - Server State Protection**:

```lisp
(defun test-server-state-protection ()
  (let ((session (make-test-session)))
    ;; Try to redefine server function
    (evaluate session "(defun handle-initialize (&rest args) :hacked)")

    ;; Server should still work
    (let ((response (send-initialize-request)))
      (assert (not (equal :hacked response))))))
```

**Generator**: Generate random Common Lisp definition forms (defun, defvar, defmacro, defclass)

**Assertion**:
- Definitions are available in subsequent evaluations within same session
- Definitions are not available in different sessions
- Server protocol functions remain unaffected

**Shrinking**: Find minimal definition that violates isolation
