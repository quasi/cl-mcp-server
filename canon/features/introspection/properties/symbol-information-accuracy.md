---
type: property
name: symbol-information-accuracy
version: 0.1.0
feature: introspection
covers:
  - contracts/describe-symbol-tool
relates_to:
  - scenarios/symbol-inspection
---

# Symbol Information Accuracy Property

## Statement

**For all** symbols that exist in a package,
**if** `describe-symbol` is called on that symbol,
**then** the returned information MUST accurately reflect the symbol's current state in the Lisp image.

## Formal Expression

```
∀ symbol ∈ Symbols, ∀ package ∈ Packages :
  (symbol ∈ package) ∧ reachable(symbol, package) ⟹
    let info = describe-symbol(symbol, package)
    then consistent(info, lisp-image-state(symbol))

where:
  consistent(info, state) ≡
    (info.type = actual-type(symbol)) ∧
    (info.boundp ⟹ info.value = symbol-value(symbol)) ∧
    (info.fboundp ⟹ info.arglist = function-lambda-list(symbol)) ∧
    (info.documentation = documentation(symbol, info.type))

  actual-type(sym) ∈ {function, macro, generic-function, class, variable, symbol}
```

## Informal Explanation

When `describe-symbol` reports information about a symbol, that information must match what would be observed by directly querying the symbol in the Lisp REPL:

1. **Type Accuracy**: The reported type (function, macro, variable, etc.) must match the symbol's actual type
2. **Value Accuracy**: If the symbol is bound to a value, that value must be current
3. **Arglist Accuracy**: Function/macro arglists must reflect the actual parameter lists
4. **Documentation Accuracy**: Docstrings must be the actual documentation from the image
5. **Source Location Accuracy**: Reported source locations must match SBCL's introspection data

This property ensures Claude receives accurate information for reasoning about code.

## Rationale

Introspection tools are Claude's eyes into the Lisp environment. Inaccurate information leads to:
- Incorrect assumptions about function signatures
- Wrong expectations about variable values
- Faulty reasoning about code structure
- Invalid suggestions and edits

The information must be "fresh" - it should reflect the current state, not cached stale data.

## Counterexample Shape

If this property is violated, you might see:

**Type Misclassification**:
```lisp
;; Actual state
(defmacro my-macro (x) `(list ,x))

;; Wrong report
describe-symbol("my-macro") → [FUNCTION]  ; VIOLATION! Should be [MACRO]
```

**Stale Value**:
```lisp
;; Initial state
(defparameter *counter* 0)

;; State changes
(incf *counter*)  ; Now *counter* = 1

;; Wrong report
describe-symbol("*counter*") → Value: 0  ; VIOLATION! Should be 1
```

**Wrong Arglist**:
```lisp
;; Actual definition
(defun add (a b) (+ a b))

;; Wrong report
describe-symbol("add") → Arglist: (X Y)  ; VIOLATION! Should be (A B)
```

**Missing Information**:
```lisp
;; Actual state
(defun documented-fn (x)
  "This function does X"
  (process x))

;; Wrong report
describe-symbol("documented-fn") → Documentation: <none>  ; VIOLATION!
```

## Verification Approach

**Generator**: Create symbols with known properties (type, value, arglist, docstring)

**Assertion**:
```lisp
(defun verify-symbol-information-accuracy (symbol-spec)
  ;; Create test symbol with known properties
  (setup-symbol symbol-spec)

  ;; Query with describe-symbol
  (let ((info (describe-symbol (symbol-spec-name symbol-spec)
                               (symbol-spec-package symbol-spec))))

    ;; Verify each aspect
    (and (equal (info-type info)
                (symbol-spec-expected-type symbol-spec))
         (equal (info-arglist info)
                (symbol-spec-expected-arglist symbol-spec))
         (equal (info-value info)
                (symbol-spec-expected-value symbol-spec))
         (equal (info-documentation info)
                (symbol-spec-expected-doc symbol-spec)))))
```

**Property Test Strategy**:

1. **Type Classification Test**:
   ```lisp
   ;; Test all type categories
   (defun test-type-accuracy ()
     (flet ((verify-type (def-form expected-type)
              (eval def-form)
              (let ((info (describe-symbol ...)))
                (assert (eq (info-type info) expected-type)))))

       (verify-type '(defun test-fn () 42) :function)
       (verify-type '(defmacro test-mac () 42) :macro)
       (verify-type '(defgeneric test-gf (x)) :generic-function)
       (verify-type '(defclass test-class () ()) :class)
       (verify-type '(defvar test-var 42) :variable)))
   ```

2. **Dynamic Value Test**:
   ```lisp
   ;; Variable values must reflect current state
   (defparameter *test-var* 100)
   (assert (equal (get-symbol-value "*test-var*") 100))

   (setf *test-var* 200)
   (assert (equal (get-symbol-value "*test-var*") 200))
   ```

3. **Arglist Consistency Test**:
   ```lisp
   ;; Cross-check with sb-introspect directly
   (defun test-arglist-accuracy (fn-name)
     (let ((tool-arglist (get-symbol-arglist fn-name))
           (direct-arglist (sb-introspect:function-lambda-list
                            (fdefinition (intern fn-name)))))
       (assert (equal tool-arglist direct-arglist))))
   ```

4. **Documentation Consistency Test**:
   ```lisp
   ;; Verify documentation matches direct query
   (defun test-doc-accuracy (sym-name type)
     (let ((tool-doc (get-symbol-doc sym-name))
           (direct-doc (documentation (intern sym-name) type)))
       (assert (equal tool-doc direct-doc))))
   ```

5. **Source Location Test**:
   ```lisp
   ;; Verify source location matches sb-introspect
   (defun test-source-accuracy (sym-name)
     (let ((tool-source (get-symbol-source sym-name))
           (direct-source (sb-introspect:find-definition-sources-by-name
                           (intern sym-name) :function)))
       (assert (matches-source tool-source direct-source))))
   ```

**Edge Cases**:
- Symbols bound to unprintable values (should handle gracefully)
- Generic functions with no methods (arglist should still be available)
- Symbols with multiple definition sources (should report primary)
- Built-in symbols with SBCL-internal source paths
- Symbols redefined after initial query (should show new state)

**Cross-Reference Test**:
```lisp
;; Verify describe-symbol agrees with REPL
(defun test-repl-consistency (symbol-name)
  (let ((repl-output (capture-repl-describe symbol-name))
        (tool-output (describe-symbol symbol-name)))
    (assert (information-matches repl-output tool-output))))
```

**Shrinking**: Find minimal symbol definition that produces inaccurate report
