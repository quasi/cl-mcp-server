---
type: property
name: compilation-safety
version: 0.1.0
feature: enhanced-evaluation
covers:
  - contracts/compile-form-tool
---

# Compilation Safety Property

## Statement

**For all** `compile-form` tool invocations,
**the tool** MUST compile code without executing any user code or modifying session state.

## Formal Expression

```
∀ code ∈ ValidLispCode, ∀ state₀ ∈ SessionState :
  let (result, state₁) = compile-form(code, state₀)
  then:
    1. No user code executed during compilation
    2. observable_state(state₁) = observable_state(state₀)
    3. All compiler diagnostics captured in result

where:
  observable_state(s) = {
    function_definitions(s),
    variable_bindings(s),
    side_effects(s),
    output_streams(s)
  }
```

## Informal Explanation

The `compile-form` tool provides safe compilation checking:

1. **No execution**: User code is never evaluated
2. **No side effects**: No printing, file I/O, or state changes
3. **State preservation**: Session remains unchanged
4. **Pure analysis**: Only compiler diagnostics produced

This allows Claude to check code for errors without risk.

## Rationale

Compilation must be side-effect-free to be safely used for:
- Pre-flight checking before evaluation
- Exploring hypothetical code
- Batch error checking

If compilation executed code, it would:
- Risk unwanted side effects
- Make the tool unsafe for speculative use
- Violate principle of least surprise

## Counterexample Shape

If this property is violated:

**Execution During Compilation**:
```lisp
;; User calls compile-form
(compile-form "(defun test () (print 'EXECUTED))")

;; Terminal shows: "EXECUTED"  ; VIOLATION!
;; Compilation should not print
```

**State Modification**:
```lisp
;; Before
(fboundp 'my-new-function) → NIL

;; User calls compile-form
(compile-form "(defun my-new-function () 42)")

;; After
(fboundp 'my-new-function) → T  ; VIOLATION!
;; Function should not be defined in image
```

**Side Effects**:
```lisp
;; File does not exist
(probe-file "/tmp/test.txt") → NIL

;; Compile code with side effects
(compile-form "(with-open-file (f \"/tmp/test.txt\" :direction :output)
                  (write-line \"data\" f))")

;; After
(probe-file "/tmp/test.txt") → #P"/tmp/test.txt"  ; VIOLATION!
;; File should not be created
```

## Verification Approach

**Generator**: Generate random valid Lisp code with potential side effects

**Assertion**:
```lisp
(defun verify-compilation-safety (code)
  ;; Capture state before
  (let ((defined-functions-before (list-all-functions))
        (special-vars-before (list-all-special-vars))
        (packages-before (list-all-packages))
        (output-captured (make-string-output-stream)))

    ;; Redirect output to detect any printing
    (let ((*standard-output* output-captured)
          (*error-output* output-captured))

      ;; Compile code
      (compile-form code)

      ;; Verify no state changes
      (and (equal defined-functions-before (list-all-functions))
           (equal special-vars-before (list-all-special-vars))
           (equal packages-before (list-all-packages))
           (zerop (length (get-output-stream-string output-captured)))))))
```

**Property Test Strategy**:

1. **Print Statement Test**:
   ```lisp
   ;; Code with print side effects
   (compile-form "(defun f () (print 'hello))")
   ;; Verify nothing printed during compilation
   ```

2. **File I/O Test**:
   ```lisp
   ;; Code with file side effects
   (compile-form "(with-open-file (f \"/tmp/test\" :direction :output)
                     (write-line \"data\" f))")
   ;; Verify file not created
   ```

3. **Function Definition Test**:
   ```lisp
   ;; Verify function not defined after compilation
   (compile-form "(defun new-func () 42)")
   (assert (not (fboundp 'new-func)))
   ```

4. **Variable Binding Test**:
   ```lisp
   ;; Verify variable not bound
   (compile-form "(defvar *new-var* 123)")
   (assert (not (boundp '*new-var*)))
   ```

5. **Macro Expansion Test**:
   ```lisp
   ;; Code that would have side effects when expanded
   (compile-form "(defmacro side-effect-macro () (print 'expanded) 42)")
   ;; Verify print did not occur during macro definition
   ```

**Edge Cases**:

- Code with reader macros that have side effects
- Code that modifies packages during read-time
- Code with `#.` read-time evaluation
- Code with `eval-when` clauses

**Implementation Requirements**:

```lisp
;; Safe compilation wrapper
(defun safe-compile-form (code)
  ;; Compile in null lexical environment
  (let ((*macroexpand-hook* #'funcall))  ; No side effects
    (with-compilation-unit (:override t)
      (handler-case
          ;; Compile without defining
          (compile nil (read-from-string
                        (format nil "(lambda () ~A)" code)))
        (error (e)
          (format nil "Compilation error: ~A" e))))))
```

**Shrinking**: Find minimal code that causes state modification during compilation
