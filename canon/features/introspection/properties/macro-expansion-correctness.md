---
type: property
name: macro-expansion-correctness
version: 0.1.0
feature: introspection
covers:
  - contracts/macroexpand-form-tool
relates_to:
  - scenarios/macro-understanding
---

# Macro Expansion Correctness Property

## Statement

**For all** macro forms,
**if** `macroexpand-form` is called with a valid macro invocation,
**then** the expansion MUST be semantically equivalent to what the Lisp compiler would expand.

## Formal Expression

```
∀ form ∈ ValidMacroForms :
  let tool_expansion = macroexpand-form(form, full: false)
      lisp_expansion = (macroexpand-1 form)
  then semantically-equal(tool_expansion, lisp_expansion)

∀ form ∈ ValidMacroForms :
  let tool_expansion = macroexpand-form(form, full: true)
      lisp_expansion = (macroexpand form)
  then semantically-equal(tool_expansion, lisp_expansion)

where:
  semantically-equal(e1, e2) ≡
    (equal e1 e2) ∨
    (pretty-print(e1) = pretty-print(e2))
```

## Informal Explanation

The `macroexpand-form` tool must produce the same expansions that the Lisp compiler would generate:

1. **Single-step mode** (`full: false`): Must match `macroexpand-1` exactly
2. **Full mode** (`full: true`): Must match recursive `macroexpand` exactly
3. **Non-macro forms**: Must return unchanged (with indication it's not a macro)
4. **Expansion order**: Must respect lexical environment and macro precedence

The expansion should be **identical** to what would result from calling the expansion functions directly, aside from formatting differences.

## Rationale

Claude uses macro expansions to:
- Understand what high-level constructs (LOOP, WITH-*, etc.) actually do
- Reason about code behavior
- Debug macro-heavy code
- Learn macro patterns

If expansions are incorrect:
- Claude misunderstands code behavior
- Suggestions based on expansion are wrong
- Debugging assistance is misleading
- Learning from expansion patterns teaches wrong things

## Counterexample Shape

If this property is violated, you might see:

**Wrong Expansion**:
```lisp
;; Actual macro
(defmacro my-when (test &body body)
  `(if ,test (progn ,@body)))

;; Input
(my-when (> x 10) (print x))

;; Wrong expansion
tool: (when (> x 10) (print x))     ; VIOLATION! Should expand to IF
lisp: (if (> x 10) (progn (print x)))  ; Correct
```

**Incomplete Expansion (full mode)**:
```lisp
;; Input with nested macros
(push item list)

;; Wrong expansion (full: true)
tool: (setq list (cons item list))  ; Partially expanded

;; Correct full expansion
lisp: (setq list (cons item list))  ; Actually correct here
```

**Macro vs Function Confusion**:
```lisp
;; Regular function
(+ 1 2)

;; Wrong behavior
tool: (expands to something different)  ; VIOLATION!

;; Correct behavior
tool: (+ 1 2) [Form is not a macro call]  ; Returns unchanged
```

**Environment-Sensitive Macro**:
```lisp
;; Macro that checks environment
(defmacro environment-aware ()
  (if (boundp '*special-mode*)
      ''mode-enabled
      ''mode-disabled))

;; Must respect current environment
tool expansion: mode-disabled  ; Should match actual *special-mode* state
```

## Verification Approach

**Generator**: Generate valid macro forms (both built-in and user-defined)

**Assertion**:
```lisp
(defun verify-macro-expansion-correctness (form full)
  ;; Expand with tool
  (let ((tool-result (macroexpand-form (write-to-string form) full)))

    ;; Expand with Lisp
    (let ((lisp-result (if full
                           (macroexpand form)
                           (macroexpand-1 form))))

      ;; Compare (allowing for print format differences)
      (assert (expansion-equivalent-p
               (parse-expansion tool-result)
               lisp-result)))))
```

**Property Test Strategy**:

1. **Built-in Macro Test**:
   ```lisp
   (defun test-builtin-macros ()
     ;; Test standard macros
     (dolist (form '((when t 42)
                     (unless nil 42)
                     (defun foo (x) x)
                     (loop for i from 1 to 10 collect i)
                     (with-open-file (s "test") (read s))
                     (push x list)
                     (incf counter)))

       (verify-expansion form :full nil)
       (verify-expansion form :full t)))
   ```

2. **User-Defined Macro Test**:
   ```lisp
   (defun test-user-macros ()
     ;; Define test macro
     (eval '(defmacro test-macro (x)
              `(list ,x ,x)))

     ;; Test expansion
     (let ((form '(test-macro 42)))
       (verify-expansion form :full nil)))
   ```

3. **Single-Step vs Full Expansion**:
   ```lisp
   (defun test-expansion-modes ()
     ;; Nested macro: PUSH expands to SETF, etc.
     (let ((form '(push item stack)))

       ;; Single step
       (let ((step1 (macroexpand-1 form)))
         (assert (equal step1
                       (parse-tool-output
                        (macroexpand-form "..." :full nil)))))

       ;; Full expansion
       (let ((full (macroexpand form)))
         (assert (equal full
                       (parse-tool-output
                        (macroexpand-form "..." :full t)))))))
   ```

4. **Non-Macro Form Test**:
   ```lisp
   (defun test-non-macro-forms ()
     ;; Regular function calls should return unchanged
     (dolist (form '((+ 1 2)
                     (list 'a 'b 'c)
                     (funcall #'identity 42)))

       (let ((result (macroexpand-form (write-to-string form))))
         (assert (form-unchanged-p result form))
         (assert (indicates-not-macro-p result)))))
   ```

5. **Direct Comparison Test**:
   ```lisp
   (defun test-direct-comparison (form-string)
     ;; Parse form
     (let ((form (read-from-string form-string)))

       ;; Expand both ways
       (let ((tool-expansion (parse-tool-output
                              (macroexpand-form form-string)))
             (lisp-expansion (macroexpand-1 form)))

         ;; Must be equal
         (assert (equal tool-expansion lisp-expansion)))))
   ```

6. **Complex Macro Test**:
   ```lisp
   (defun test-complex-macros ()
     ;; LOOP is notoriously complex
     (let ((form '(loop for i from 1 to 10
                       when (evenp i)
                       collect (* i i))))

       ;; Full expansion should match
       (verify-expansion form :full t)))
   ```

7. **Recursive Macro Test**:
   ```lisp
   (defun test-recursive-macros ()
     ;; Define mutually recursive macros
     (eval '(defmacro mac-a (x)
              (if (zerop x)
                  x
                  `(mac-b ,(1- x)))))

     (eval '(defmacro mac-b (x)
              (if (zerop x)
                  x
                  `(mac-a ,(1- x)))))

     ;; Test full expansion terminates correctly
     (let ((form '(mac-a 5)))
       (verify-expansion form :full t)))
   ```

8. **Package Context Test**:
   ```lisp
   (defun test-package-context ()
     ;; Define macro in specific package
     (in-package :my-test-package)
     (defmacro pkg-macro (x) `(list ,x))

     ;; Expansion must respect package
     (let ((form '(pkg-macro 42)))
       (verify-expansion form :full nil)))
   ```

**Edge Cases**:
- Macros that expand to other macros (nested)
- Compiler macros (may have different expansion rules)
- Symbol macros (symbol-level macros)
- Macros with &whole, &environment parameters
- Macros that inspect their lexical environment
- Macros with side effects during expansion (should not execute them)

**Format Tolerance**:
```lisp
;; These should be considered equivalent:
(if t (progn (print x)))

(IF T
  (PROGN
    (PRINT X)))

;; Different formatting, same structure
```

**Implementation Notes**:
```lisp
;; Tool must use:
(macroexpand-1 form)  ; for single-step
(macroexpand form)    ; for full expansion

;; NOT:
(eval form)           ; would execute, not expand
```

**Shrinking**: Find minimal macro form that produces incorrect expansion
