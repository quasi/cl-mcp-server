---
type: property
name: cross-reference-correctness
version: 0.1.0
feature: introspection
covers:
  - contracts/who-calls-tool
  - contracts/who-references-tool
relates_to:
  - scenarios/cross-reference
---

# Cross-Reference Correctness Property

## Statement

**For all** compiled functions and variables,
**if** function A calls function B (or references variable V),
**then** the cross-reference tools MUST report this relationship.

## Formal Expression

```
∀ fn_A, fn_B ∈ CompiledFunctions :
  calls(fn_A, fn_B) ⟹ fn_A ∈ who-calls(fn_B)

∀ fn ∈ CompiledFunctions, ∀ var ∈ SpecialVariables :
  references(fn, var) ⟹ fn ∈ who-references(var)

where:
  calls(A, B) ≡ ∃ instruction in bytecode(A) : instruction invokes B
  references(fn, var) ≡ ∃ instruction in bytecode(fn) : instruction reads var

  CompiledFunctions = functions with xref data populated
```

## Informal Explanation

When code is compiled, SBCL maintains a cross-reference database tracking:
- Which functions call which other functions
- Which functions reference (read) which special variables

The `who-calls` and `who-references` tools query this database. This property states:

1. **Completeness**: If a relationship exists in compiled code, the tool must report it
2. **Soundness**: If the tool reports a relationship, it must actually exist
3. **Freshness**: Newly compiled code should update the xref database
4. **Accuracy**: Results must match SBCL's internal xref data

This enables reliable code navigation and impact analysis.

## Rationale

Cross-reference data is essential for:
- Understanding code dependencies
- Impact analysis for changes
- Navigation during code exploration
- Identifying unused code

Incorrect xref data leads to:
- Missing dependencies in impact analysis
- False positives causing unnecessary investigation
- Inability to trace code flow
- Wrong conclusions about code usage

The property is limited to **compiled** code because SBCL only populates xref data during compilation, not interpretation.

## Counterexample Shape

If this property is violated, you might see:

**Missing Relationship (False Negative)**:
```lisp
;; Actual code
(defun caller-fn ()
  (mapcar #'identity '(1 2 3)))

(compile 'caller-fn)

;; Wrong report
who-calls("mapcar") → []  ; VIOLATION! Should include CALLER-FN
```

**Phantom Relationship (False Positive)**:
```lisp
;; No code calls my-lonely-function
(defun my-lonely-function () 42)

;; Wrong report
who-calls("my-lonely-function") → (PHANTOM-CALLER)  ; VIOLATION!
```

**Stale Data**:
```lisp
;; Original version
(defun old-caller () (some-function))

;; After recompilation (removed the call)
(defun old-caller () 42)
(compile 'old-caller)

;; Wrong report
who-calls("some-function") → (OLD-CALLER)  ; VIOLATION! Call no longer exists
```

**Variable Reference Missing**:
```lisp
;; Code that references *print-base*
(defun my-printer (x)
  (format nil "~v,48R" *print-base* x))

(compile 'my-printer)

;; Wrong report
who-references("*print-base*") → []  ; VIOLATION! Should include MY-PRINTER
```

## Verification Approach

**Generator**: Create functions with known call patterns

**Assertion**:
```lisp
(defun verify-cross-reference (caller callee)
  ;; Define and compile caller
  (eval `(defun ,caller () (,callee)))
  (compile caller)

  ;; Verify who-calls reports the relationship
  (let ((callers (who-calls (symbol-name callee))))
    (assert (member (symbol-name caller) callers :test #'string-equal))))
```

**Property Test Strategy**:

1. **Who-Calls Completeness Test**:
   ```lisp
   (defun test-who-calls-completeness ()
     ;; Create caller -> callee relationship
     (eval '(defun test-caller-fn () (test-callee-fn)))
     (eval '(defun test-callee-fn () 42))
     (compile 'test-caller-fn)

     ;; Verify reported by tool
     (let ((result (who-calls "test-callee-fn")))
       (assert (find "TEST-CALLER-FN" result :test #'string-equal))))
   ```

2. **Who-Calls Soundness Test**:
   ```lisp
   (defun test-who-calls-soundness ()
     ;; Query who-calls for a function
     (let ((callers (who-calls "mapcar" "CL")))

       ;; Verify each reported caller actually calls the function
       (dolist (caller callers)
         (assert (actually-calls-p caller "mapcar")))))
   ```

3. **Who-References Test**:
   ```lisp
   (defun test-who-references ()
     ;; Create function that references variable
     (eval '(defvar *test-var* 100))
     (eval '(defun referencer () (print *test-var*)))
     (compile 'referencer)

     ;; Verify relationship reported
     (let ((refs (who-references "*test-var*")))
       (assert (find "REFERENCER" refs :test #'string-equal))))
   ```

4. **Freshness Test** (newly compiled code):
   ```lisp
   (defun test-xref-freshness ()
     ;; Initial state: no callers
     (defun fresh-callee () 42)
     (compile 'fresh-callee)
     (assert (null (who-calls "fresh-callee")))

     ;; Add caller
     (defun fresh-caller () (fresh-callee))
     (compile 'fresh-caller)

     ;; Verify xref updated
     (let ((result (who-calls "fresh-callee")))
       (assert (find "FRESH-CALLER" result :test #'string-equal))))
   ```

5. **Direct Comparison with SBCL**:
   ```lisp
   (defun test-sbcl-consistency (fn-name)
     ;; Get xref data directly from SBCL
     (let* ((symbol (find-symbol fn-name))
            (sbcl-callers (sb-introspect:who-calls symbol))
            (tool-callers (who-calls fn-name)))

       ;; Tool results should match SBCL's data
       (assert (set-equal (normalize-names sbcl-callers)
                          (parse-tool-output tool-callers)))))
   ```

6. **Recompilation Test** (xref updates correctly):
   ```lisp
   (defun test-xref-updates-on-recompile ()
     ;; Version 1: calls function-a
     (eval '(defun changing-caller () (function-a)))
     (compile 'changing-caller)
     (assert (member "CHANGING-CALLER" (who-calls "function-a")
                     :test #'string-equal))

     ;; Version 2: calls function-b instead
     (eval '(defun changing-caller () (function-b)))
     (compile 'changing-caller)

     ;; Verify xref updated
     (assert (not (member "CHANGING-CALLER" (who-calls "function-a")
                          :test #'string-equal)))
     (assert (member "CHANGING-CALLER" (who-calls "function-b")
                     :test #'string-equal)))
   ```

**Edge Cases**:
- Interpreted (non-compiled) code may not have xref data
- Macros that expand to function calls (xref tracks expansion)
- Indirect calls through `funcall` or `apply` (may not be tracked)
- Cross-package calls (should report with qualified names)
- Recursive functions (function calls itself)
- Multiple callers of the same function

**Limitations to Document**:
```lisp
;; These may NOT be tracked by xref:
(funcall 'some-function)      ; indirect call
(apply #'some-function args)  ; apply
(eval '(some-function))       ; eval

;; Only direct calls in compiled code are reliably tracked:
(some-function)               ; tracked
```

**Implementation Notes**:
```lisp
;; Must use sb-introspect's xref API
(sb-introspect:who-calls symbol)      ; returns list of callers
(sb-introspect:who-references symbol) ; returns list of referencers
```

**Shrinking**: Find minimal function pair that produces incorrect xref result
