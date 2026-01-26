---
type: property
name: symbol-discovery-completeness
version: 0.1.0
feature: introspection
covers:
  - contracts/apropos-search-tool
relates_to:
  - scenarios/symbol-discovery
---

# Symbol Discovery Completeness Property

## Statement

**For all** symbols that exist in accessible packages,
**if** the symbol name matches the search pattern and optional filters,
**then** `apropos-search` MUST include that symbol in its results.

## Formal Expression

```
∀ symbol ∈ AccessibleSymbols, ∀ pattern ∈ Patterns :
  matches(symbol.name, pattern) ∧
  satisfies-filters(symbol, filters) ⟹
    symbol ∈ apropos-search(pattern, filters)

where:
  matches(name, pattern) ≡
    contains-substring(lowercase(name), lowercase(pattern))

  satisfies-filters(sym, filters) ≡
    (¬filters.package ∨ sym.package = filters.package) ∧
    (¬filters.type ∨ symbol-type(sym) = filters.type)

  AccessibleSymbols =
    if filters.package specified:
      all-symbols-in-package(filters.package)
    else:
      external-symbols-in-all-packages()
```

## Informal Explanation

The `apropos-search` tool must find **all** matching symbols, not just some:

1. **Case-Insensitive Matching**: "MAP" finds "mapcar", "Map-Over", "mapping"
2. **Substring Matching**: Pattern anywhere in symbol name
3. **Scope Rules**:
   - Without package filter: Search external symbols of all packages
   - With package filter: Search all symbols (including internal) in that package
4. **Type Filtering**: When type specified, only show symbols of that type
5. **Completeness**: No false negatives (missing matches)
6. **Soundness**: No false positives (non-matching results)

This enables reliable symbol discovery for Claude's code exploration.

## Rationale

Claude uses `apropos-search` to:
- Discover available functions in a domain ("serialize")
- Find related symbols ("*print*" finds all print-related variables)
- Explore API surface of packages
- Learn naming patterns in the codebase

If search is incomplete:
- Claude misses relevant functions
- API exploration is unreliable
- Suggestions might reinvent existing functionality
- Code navigation is impaired

The property ensures search is trustworthy: if it's not in the results, it doesn't match.

## Counterexample Shape

If this property is violated, you might see:

**Missing Match (False Negative)**:
```lisp
;; Actual symbols in CL package
(find-symbol "MAPCAR" :cl) → MAPCAR, :EXTERNAL

;; Wrong search result
apropos-search("map") → [..., no MAPCAR, ...]
;; VIOLATION! MAPCAR matches but not in results
```

**Incorrect Filtering**:
```lisp
;; Actual symbol types
(fboundp 'mapcar) → T  ; it's a function

;; Wrong filtered search
apropos-search("mapcar", type: "function") → []
;; VIOLATION! MAPCAR is a function, should be included
```

**Package Scope Violation**:
```lisp
;; Internal symbol in package
(find-symbol "INTERNAL-HELPER" :my-package) → INTERNAL-HELPER, :INTERNAL

;; Wrong scoped search
apropos-search("internal", package: "my-package") → []
;; VIOLATION! Should find internal symbols when package specified
```

**Case Sensitivity Issue**:
```lisp
;; Symbol exists
(find-symbol "DEFUN" :cl) → DEFUN, :EXTERNAL

;; Wrong search
apropos-search("defun") → []  ; VIOLATION! Should be case-insensitive
apropos-search("DEFUN") → [DEFUN]  ; Works, but inconsistent
```

## Verification Approach

**Generator**: Generate search patterns and verify against known symbol sets

**Assertion**:
```lisp
(defun verify-symbol-discovery-completeness (pattern filters)
  ;; Get expected symbols by direct enumeration
  (let ((expected (enumerate-matching-symbols pattern filters))
        (actual (parse-apropos-results
                 (apropos-search pattern filters))))

    ;; Verify all expected symbols found
    (assert (subsetp expected actual :test #'symbol-equal))

    ;; Verify no unexpected symbols (soundness)
    (assert (subsetp actual expected :test #'symbol-equal))))
```

**Property Test Strategy**:

1. **Exhaustive Small Package Test**:
   ```lisp
   (defun test-small-package-completeness ()
     ;; Create test package with known symbols
     (defpackage :test-discovery
       (:use :cl)
       (:export #:foo-bar #:foo-baz #:quux))

     (in-package :test-discovery)
     (defun foo-bar () 1)
     (defun foo-baz () 2)
     (defun quux () 3)

     ;; Search for "foo"
     (let ((results (apropos-search "foo" :package "TEST-DISCOVERY")))
       ;; Must find both FOO-BAR and FOO-BAZ
       (assert (find "FOO-BAR" results :test #'search))
       (assert (find "FOO-BAZ" results :test #'search))
       (assert (not (find "QUUX" results :test #'search)))))
   ```

2. **Case Insensitivity Test**:
   ```lisp
   (defun test-case-insensitivity ()
     ;; All these should find MAPCAR
     (dolist (pattern '("map" "MAP" "Map" "MaP"))
       (let ((results (apropos-search pattern :package "CL")))
         (assert (find "MAPCAR" results :test #'search)))))
   ```

3. **Type Filtering Test**:
   ```lisp
   (defun test-type-filtering ()
     ;; Search for "def" with type filters
     (let ((all-defs (apropos-search "def" :package "CL"))
           (macros (apropos-search "def" :package "CL" :type "macro"))
           (functions (apropos-search "def" :package "CL" :type "function")))

       ;; DEFUN should be in macros, not functions
       (assert (find "DEFUN" macros :test #'search))
       (assert (not (find "DEFUN" functions :test #'search)))

       ;; All filtered results should be in unfiltered results
       (assert (subsetp (parse-symbols macros)
                       (parse-symbols all-defs)))))
   ```

4. **Package Scoping Test**:
   ```lisp
   (defun test-package-scoping ()
     ;; Without package: only external symbols
     (let ((external-only (apropos-search "map")))

       ;; With package: includes internal symbols
       (in-package :test-package)
       (defun internal-mapper () nil)  ; not exported

       (let ((all-symbols (apropos-search "map" :package "TEST-PACKAGE")))

         ;; Internal symbol should appear only in scoped search
         (assert (find "INTERNAL-MAPPER" all-symbols :test #'search))

         ;; External-only search should not include it
         (assert (not (find "INTERNAL-MAPPER" external-only :test #'search))))))
   ```

5. **Cross-Reference with DO-SYMBOLS**:
   ```lisp
   (defun test-against-do-symbols (pattern package)
     ;; Manually enumerate matching symbols
     (let ((expected nil))
       (if package
           (do-symbols (sym (find-package package))
             (when (search pattern (symbol-name sym) :test #'char-equal)
               (push sym expected)))
           (do-all-symbols (sym)
             (when (and (eq (symbol-package sym)
                           (find-package (package-name (symbol-package sym))))
                       (search pattern (symbol-name sym) :test #'char-equal))
               (push sym expected))))

       ;; Compare with tool results
       (let ((tool-results (parse-apropos-results
                            (apropos-search pattern :package package))))

         (assert (set-equal expected tool-results)))))
   ```

6. **Empty Pattern Test**:
   ```lisp
   (defun test-empty-pattern ()
     ;; Empty pattern should match everything (though may be limited)
     (let ((results (apropos-search "" :package "CL")))
       ;; Should find well-known symbols
       (assert (find "MAPCAR" results :test #'search))
       (assert (find "DEFUN" results :test #'search))))
   ```

7. **Special Character Test**:
   ```lisp
   (defun test-special-characters ()
     ;; Symbols with special characters
     (let ((star-results (apropos-search "*" :package "CL")))
       ;; Should find *print-base*, *standard-output*, etc.
       (assert (find "*PRINT-BASE*" star-results :test #'search))
       (assert (find "*STANDARD-OUTPUT*" star-results :test #'search)))

     ;; Plus signs
     (let ((plus-results (apropos-search "+" :package "CL")))
       (assert (find "+" plus-results :test #'search))
       (assert (find "1+" plus-results :test #'search))))
   ```

8. **Substring Position Test**:
   ```lisp
   (defun test-substring-position ()
     ;; Pattern matches anywhere in symbol name
     (let ((results (apropos-search "car" :package "CL")))
       ;; Should find symbols with "car" anywhere
       (assert (find "MAPCAR" results :test #'search))    ; at end
       (assert (find "CAR" results :test #'search))       ; whole name
       (assert (find "NTHCDR" results :test #'search))))  ; in middle
   ```

9. **Type Classification Consistency**:
   ```lisp
   (defun test-type-classification ()
     ;; Type filter must match describe-symbol classification
     (let ((macros (parse-apropos-results
                    (apropos-search "def" :package "CL" :type "macro"))))

       (dolist (macro-name macros)
         ;; Each should be classified as macro by describe-symbol
         (let ((desc (describe-symbol macro-name "CL")))
           (assert (search "[MACRO]" desc))))))
   ```

10. **Duplicate Elimination Test**:
    ```lisp
    (defun test-no-duplicates ()
      ;; Results should not contain duplicates
      (let ((results (parse-apropos-results
                      (apropos-search "map" :package "CL"))))

        (assert (= (length results)
                  (length (remove-duplicates results :test #'string-equal))))))
    ```

**Edge Cases**:
- Empty pattern (matches all symbols, may need limiting)
- Single character patterns ("*", "+", "-")
- Very common substrings ("A", "E")
- Symbols with non-alphanumeric characters
- Symbols in keyword package
- Symbols that shadow built-ins
- Uninterned symbols (should not appear)

**Performance Considerations**:
```lisp
;; For large result sets, consider pagination
(when (> result-count 1000)
  (warn "Very large result set, consider narrowing pattern"))
```

**Implementation Notes**:

Search scope implementation:
```lisp
(defun search-symbols (pattern package-name type-filter)
  (let ((matches nil))
    (if package-name
        ;; Scoped: all symbols in package
        (do-symbols (sym (find-package package-name))
          (when (and (pattern-matches sym pattern)
                    (type-matches sym type-filter))
            (push sym matches)))
        ;; Unscoped: only external symbols
        (do-all-symbols (sym)
          (when (and (external-symbol-p sym)
                    (pattern-matches sym pattern)
                    (type-matches sym type-filter))
            (push sym matches))))
    (sort matches #'string< :key #'symbol-name)))
```

Pattern matching:
```lisp
(defun pattern-matches (symbol pattern)
  (search pattern (symbol-name symbol) :test #'char-equal))
```

**Shrinking**: Find minimal pattern+filter combination that misses a matching symbol
