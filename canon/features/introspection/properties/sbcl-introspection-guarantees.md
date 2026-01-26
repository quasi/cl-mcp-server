---
type: property
name: sbcl-introspection-guarantees
version: 0.1.0
feature: introspection
covers:
  - contracts/describe-symbol-tool
  - contracts/who-calls-tool
  - contracts/who-references-tool
relates_to:
  - vocabulary#SBCL-Introspect
---

# SBCL Introspection Guarantees Property

## Statement

**For all** introspection operations,
**the tools** MUST use SBCL's `sb-introspect` package APIs and MUST NOT fail due to SBCL version differences in the supported SBCL versions (2.0+).

## Formal Expression

```
∀ tool ∈ IntrospectionTools, ∀ input ∈ ValidInputs :
  let result = execute(tool, input)
  then uses-sb-introspect(tool) ∧
       (∀ v ∈ SupportedSBCLVersions : compatible(tool, v))

where:
  uses-sb-introspect(tool) ≡
    ∀ fn ∈ functions-called-by(tool) :
      fn ∈ sb-introspect:external-symbols ∨
      fn ∈ common-lisp:external-symbols

  SupportedSBCLVersions = {v : v ≥ 2.0}

  compatible(tool, version) ≡
    tool executes without symbol-not-found errors
```

## Informal Explanation

This property establishes implementation constraints for introspection tools:

1. **SBCL-Specific**: Tools leverage `sb-introspect` for rich information unavailable in standard CL
2. **Version Stability**: Tools work across SBCL 2.0+ without modification
3. **Graceful Degradation**: If SBCL APIs change, tools handle missing features gracefully
4. **API Contract**: Specific sb-introspect APIs are used correctly:
   - `sb-introspect:function-lambda-list` for arglists
   - `sb-introspect:find-definition-sources-by-name` for source locations
   - `sb-introspect:who-calls` for caller queries
   - `sb-introspect:who-references` for variable reference queries

The property ensures introspection tools are:
- **Reliable**: Don't break on SBCL updates
- **Powerful**: Leverage SBCL's advanced introspection capabilities
- **Maintainable**: Use documented, stable APIs

## Rationale

SBCL's introspection capabilities far exceed standard Common Lisp:
- Source location tracking
- Cross-reference database
- Lambda list retrieval for compiled functions
- Detailed type information

But these are SBCL-specific. This property:
- Documents the SBCL dependency explicitly
- Ensures we use stable, documented APIs
- Prevents breakage from SBCL version updates
- Makes porting to other implementations clear (would need alternative approach)

## Counterexample Shape

If this property is violated, you might see:

**Undocumented API Usage**:
```lisp
;; Wrong: Using internal SBCL API
(sb-impl::internal-function-info sym)  ; VIOLATION! Using internal API

;; Correct: Using documented sb-introspect
(sb-introspect:function-lambda-list sym)  ; Uses public API
```

**Version-Specific Code**:
```lisp
;; Wrong: Brittle version check
(if (>= sb-ext:*sbcl-version* 2.3)
    (new-api-call)
    (old-api-call))  ; VIOLATION! Fragile

;; Correct: Feature detection
(if (fboundp 'sb-introspect:some-new-function)
    (funcall 'sb-introspect:some-new-function ...)
    (fallback-implementation ...))
```

**Missing Error Handling**:
```lisp
;; Wrong: Assumes API always works
(sb-introspect:find-definition-sources-by-name sym :function)
;; VIOLATION! Could fail on certain symbols

;; Correct: Handles errors gracefully
(handler-case
    (sb-introspect:find-definition-sources-by-name sym :function)
  (error (e)
    (format nil "Source unavailable: ~A" e)))
```

**Wrong API for Task**:
```lisp
;; Wrong: Using low-level API
(sb-c::fun-info sym)  ; VIOLATION! Compiler internals

;; Correct: Using appropriate high-level API
(sb-introspect:function-lambda-list (fdefinition sym))
```

## Verification Approach

**Generator**: Generate introspection queries for various symbol types

**Assertion**:
```lisp
(defun verify-sbcl-introspection-guarantees (tool-name)
  ;; 1. Verify sb-introspect usage
  (assert (uses-only-sb-introspect-apis tool-name))

  ;; 2. Verify error handling
  (assert (handles-api-errors-gracefully tool-name))

  ;; 3. Verify version compatibility
  (assert (compatible-with-sbcl-2-x tool-name)))
```

**Property Test Strategy**:

1. **API Usage Verification**:
   ```lisp
   (defun test-sb-introspect-api-usage ()
     ;; Verify tools only call documented APIs
     (let ((allowed-packages '(:sb-introspect :common-lisp :cl-user)))

       (dolist (tool '(describe-symbol who-calls who-references))
         (let ((called-functions (functions-called-by tool)))
           (dolist (fn called-functions)
             (assert (member (symbol-package fn) allowed-packages)))))))
   ```

2. **Version Compatibility Test**:
   ```lisp
   (defun test-sbcl-version-compatibility ()
     ;; Verify works on SBCL 2.0+
     (when (>= (parse-sbcl-version) 2.0)
       (assert (all-introspection-tools-work-p))))
   ```

3. **Error Handling Test**:
   ```lisp
   (defun test-error-handling ()
     ;; Query that might fail
     (let ((result (describe-symbol "nonexistent-symbol")))
       ;; Should return error message, not crash
       (assert (stringp result))
       (assert (not (search "unhandled error" result)))))
   ```

4. **Specific API Tests**:
   ```lisp
   (defun test-lambda-list-api ()
     ;; Verify correct lambda-list retrieval
     (defun test-fn (a b &optional c) nil)
     (let ((tool-arglist (get-arglist-from-describe "test-fn"))
           (direct-arglist (sb-introspect:function-lambda-list
                            (fdefinition 'test-fn))))
       (assert (equal tool-arglist direct-arglist))))

   (defun test-xref-apis ()
     ;; Verify correct xref usage
     (defun caller () (callee))
     (defun callee () 42)
     (compile 'caller)

     (let ((tool-callers (who-calls "callee"))
           (direct-callers (sb-introspect:who-calls 'callee)))
       (assert (matches-xref-data-p tool-callers direct-callers))))

   (defun test-source-location-api ()
     ;; Verify source location retrieval
     (let ((tool-source (get-source-from-describe "mapcar"))
           (direct-sources (sb-introspect:find-definition-sources-by-name
                            'mapcar :function)))
       (assert (matches-source-info-p tool-source direct-sources))))
   ```

5. **Graceful Degradation Test**:
   ```lisp
   (defun test-graceful-degradation ()
     ;; Test with symbols that have no source
     (let ((result (describe-symbol "gensym" "CL")))
       (assert (not (search "ERROR" result)))
       (assert (or (search "Source: unavailable" result)
                   (search "Source: built-in" result)))))
   ```

6. **Cross-Package Symbol Test**:
   ```lisp
   (defun test-cross-package-introspection ()
     ;; Test introspecting symbols from different packages
     (dolist (pkg-sym '(("mapcar" "CL")
                        ("evaluate-code" "CL-MCP-SERVER.EVALUATOR")
                        ("*session*" "CL-MCP-SERVER.SESSION")))
       (let ((result (describe-symbol (first pkg-sym) (second pkg-sym))))
         (assert (successful-introspection-p result)))))
   ```

**Edge Cases**:
- Built-in functions (may have SBCL-internal source paths)
- Compiled vs interpreted functions (different info available)
- Generic functions (arglist from generic, not methods)
- Symbols with no definition (should handle gracefully)
- Symbols in locked packages (should be read-only safe)

**Required SBCL APIs**:

Must use these documented APIs:
```lisp
sb-introspect:function-lambda-list          ; Get function arglist
sb-introspect:find-definition-sources-by-name  ; Get source location
sb-introspect:who-calls                     ; Get callers
sb-introspect:who-references                ; Get variable references
```

Must NOT use:
```lisp
sb-impl::*                                  ; Internal implementation
sb-c::*                                     ; Compiler internals
sb-kernel::*                                ; Kernel internals
```

**Documentation Check**:
```lisp
;; Verify APIs are in SBCL manual
(defun test-api-documented ()
  (dolist (api '(sb-introspect:function-lambda-list
                 sb-introspect:find-definition-sources-by-name
                 sb-introspect:who-calls
                 sb-introspect:who-references))
    (assert (exported-symbol-p api))
    (assert (documented-in-sbcl-manual-p api))))
```

**Implementation Notes**:

The following template ensures SBCL API correctness:
```lisp
(defun safe-sb-introspect-call (fn &rest args)
  "Safely call sb-introspect function with error handling"
  (handler-case
      (apply fn args)
    (sb-int:simple-program-error (e)
      ;; Symbol not found, etc.
      (format nil "Introspection failed: ~A" e))
    (error (e)
      ;; Other errors
      (format nil "Error: ~A" e))))
```

**Shrinking**: Find minimal introspection query that uses wrong API or fails on SBCL version
