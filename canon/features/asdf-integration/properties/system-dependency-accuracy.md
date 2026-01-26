---
type: property
name: system-dependency-accuracy
version: 0.1.0
feature: asdf-integration
covers:
  - contracts/system-dependencies-tool
  - contracts/describe-system-tool
---

# System Dependency Accuracy Property

## Statement

**For all** ASDF systems accessible in the session,
**the system-dependencies tool** MUST accurately report all direct and transitive dependencies with correct dependency relationships.

## Formal Expression

```
∀ system ∈ LoadableSystems :
  let deps_direct = system-dependencies(system, transitive=false)
  let deps_transitive = system-dependencies(system, transitive=true)
  let actual_deps = asdf:system-depends-on(system)
  then:
    1. deps_direct = actual_deps
    2. deps_transitive ⊇ deps_direct
    3. ∀ dep ∈ deps_transitive :
         ∃ path ∈ DependencyPaths : connects(system, dep, path)
    4. No spurious dependencies: deps_transitive ⊆ reachable(system)
    5. Dependency graph is acyclic (no circular deps reported)

where:
  DependencyGraph = {
    nodes: [SystemName],
    edges: [(system, dependency)],
    properties: {
      acyclic: Boolean,
      complete: Boolean
    }
  }

  reachable(s) = s ∪ ⋃{reachable(d) | d ∈ direct_deps(s)}
```

## Informal Explanation

The system dependencies tool must provide accurate dependency information:

1. **Direct Dependencies**: Exact match with ASDF system definition
2. **Transitive Dependencies**: All dependencies reachable through dependency chains
3. **Completeness**: No missing dependencies
4. **Correctness**: No spurious (non-existent) dependencies
5. **Graph Structure**: Properly represents the dependency DAG

This allows Claude to:
- Understand system relationships
- Analyze dependency impact
- Identify loading order requirements
- Detect potential conflicts

## Rationale

Accurate dependency information is essential for:
- Planning system loads
- Understanding compilation order
- Analyzing system architecture
- Debugging load failures
- Identifying circular dependencies

Incorrect dependency reporting would:
- Lead to load failures
- Hide critical dependencies
- Show phantom relationships
- Confuse dependency analysis
- Make system inspection unreliable

Key accuracy requirements:
- Direct dependencies must match ASDF definition exactly
- Transitive closure must be complete
- No dependencies should appear that aren't actually required
- Dependency cycles must be detected and reported

## Counterexample Shape

If this property is violated:

**Missing Direct Dependency**:
```lisp
;; System definition in my-system.asd:
(defsystem "my-system"
  :depends-on ("alexandria" "cl-ppcre"))

;; system-dependencies shows:
{
  "system": "my-system",
  "dependencies": ["alexandria"]
  ; VIOLATION! Missing cl-ppcre
}
```

**Spurious Dependency**:
```lisp
;; System only depends on alexandria
(defsystem "simple-system"
  :depends-on ("alexandria"))

;; system-dependencies(transitive=true) shows:
{
  "dependencies": ["alexandria", "cl-ppcre", "bordeaux-threads"]
  ; VIOLATION! cl-ppcre and bordeaux-threads are not dependencies
}
```

**Incomplete Transitive Dependencies**:
```lisp
;; system-a depends on system-b
;; system-b depends on system-c

;; system-dependencies("system-a", transitive=true) shows:
{
  "dependencies": ["system-b"]
  ; VIOLATION! Missing transitive dependency system-c
}
```

**Incorrect Dependency Relationship**:
```lisp
;; hunchentoot depends on usocket
;; drakma also depends on usocket (independent)

;; system-dependencies shows hunchentoot → drakma → usocket
;; VIOLATION! Wrong relationship, should be:
;;   hunchentoot → usocket
;;   drakma → usocket (separate edge)
```

**Circular Dependency Not Detected**:
```lisp
;; system-a depends on system-b
;; system-b depends on system-a (circular!)

;; system-dependencies("system-a") returns:
{
  "dependencies": ["system-b"],
  "circular": false
  ; VIOLATION! Should detect and report circular dependency
}
```

## Verification Approach

**Generator**: Use known ASDF systems with well-defined dependencies

**Assertion**:
```lisp
(defun verify-dependency-accuracy (system-name)
  ;; Get dependencies from tool
  (let* ((direct-deps (system-dependencies system-name :transitive nil))
         (transitive-deps (system-dependencies system-name :transitive t))

         ;; Get actual dependencies from ASDF
         (actual-direct (asdf:system-depends-on
                          (asdf:find-system system-name)))
         (actual-transitive (compute-transitive-deps system-name)))

    (and
      ;; Direct dependencies match exactly
      (set-equal direct-deps actual-direct :test #'string-equal)

      ;; Transitive includes all direct
      (subsetp direct-deps transitive-deps :test #'string-equal)

      ;; Transitive matches actual reachable deps
      (set-equal transitive-deps actual-transitive :test #'string-equal)

      ;; No dependency appears that isn't reachable
      (every (lambda (dep)
               (or (member dep actual-direct :test #'string-equal)
                   (some (lambda (d)
                           (depends-on-p d dep))
                         actual-direct)))
             transitive-deps)

      ;; Verify acyclic
      (acyclic-dependency-graph-p system-name transitive-deps))))
```

**Property Test Strategy**:

1. **Simple System**:
   ```lisp
   ;; System with known dependencies
   (verify-dependency-accuracy "alexandria")
   ;; Should show no dependencies (or only trivial ones)
   ```

2. **Complex System**:
   ```lisp
   ;; System with many dependencies
   (verify-dependency-accuracy "hunchentoot")
   ;; Should accurately show all 11+ dependencies
   ```

3. **Transitive Closure**:
   ```lisp
   ;; Verify transitive flag
   (let ((direct (system-dependencies "drakma" :transitive nil))
         (trans (system-dependencies "drakma" :transitive t)))
     (assert (< (length direct) (length trans)))
     (assert (subsetp direct trans :test #'string-equal)))
   ```

4. **Diamond Dependencies**:
   ```lisp
   ;; System A depends on B and C
   ;; Both B and C depend on D
   ;; Verify D appears once in transitive deps
   (let ((deps (system-dependencies "system-a" :transitive t)))
     (assert (= 1 (count "system-d" deps :test #'string-equal))))
   ```

5. **Dependency Count Verification**:
   ```lisp
   ;; Compare with ASDF's own dependency computation
   (defun check-against-asdf (system-name)
     (let ((tool-deps (system-dependencies system-name :transitive t))
           (asdf-deps (asdf:system-depends-on
                        (asdf:find-system system-name))))
       (set-equal tool-deps asdf-deps :test #'string-equal)))
   ```

6. **System Not Found**:
   ```lisp
   ;; Should handle missing system gracefully
   (handler-case
       (system-dependencies "nonexistent-system-xyz")
     (error (e)
       ;; Should report "system not found", not incorrect deps
       (search "not found" (princ-to-string e))))
   ```

**Edge Cases**:

- Systems with no dependencies
- Systems with optional dependencies
- Systems with conditional dependencies (platform-specific)
- Systems with :defsystem-depends-on
- Circular dependency detection
- Very deep dependency chains (10+ levels)
- Missing or unloadable dependencies

**Implementation Requirements**:

```lisp
;; Dependency graph computation
(defun compute-dependencies (system-name &key transitive)
  (let ((system (asdf:find-system system-name)))
    (if transitive
        ;; Compute transitive closure
        (let ((visited (make-hash-table :test #'equal))
              (deps '()))
          (labels ((collect-deps (sys)
                    (unless (gethash sys visited)
                      (setf (gethash sys visited) t)
                      (let ((direct-deps (asdf:system-depends-on
                                          (asdf:find-system sys))))
                        (dolist (dep direct-deps)
                          (pushnew dep deps :test #'string-equal)
                          (collect-deps dep))))))
            (collect-deps system-name))
          deps)
        ;; Just direct dependencies
        (asdf:system-depends-on system))))
```

**Shrinking**: Find minimal dependency graph that shows inaccuracy

## Related Properties

- **system-information-completeness**: System metadata is accurate
- **quickload-download-safety**: Dependencies loaded securely
- **dependency-resolution-correctness**: Transitive resolution works correctly
- **system-discovery-completeness**: All systems can be found for dependency analysis
