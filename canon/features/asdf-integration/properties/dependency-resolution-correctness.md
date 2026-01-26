---
type: property
name: dependency-resolution-correctness
version: 0.1.0
feature: asdf-integration
covers:
  - contracts/system-dependencies-tool
  - contracts/quickload-tool
  - contracts/describe-system-tool
---

# Dependency Resolution Correctness Property

## Statement

**For all** systems with transitive dependencies,
**dependency resolution** MUST correctly compute the transitive closure and determine a valid topological load order.

## Formal Expression

```
∀ system ∈ Systems, ∀ deps ∈ DependencyGraphs :
  let transitive = resolve-dependencies(system, deps)
  let load_order = compute-load-order(transitive)
  then:
    1. Transitive closure complete:
         ∀ d ∈ direct_deps(system) :
           d ∈ transitive ∧
           resolve-dependencies(d, deps) ⊆ transitive

    2. Load order valid:
         ∀ (sys, dep) ∈ deps :
           position(dep, load_order) < position(sys, load_order)

    3. No missing dependencies:
         ∀ sys ∈ transitive :
           ∀ dep ∈ direct_deps(sys) :
             dep ∈ transitive ∨ dep ∈ preloaded

    4. No circular dependencies:
         ¬∃ cycle ∈ dependency_graph(transitive)

    5. Minimal closure:
         transitive = {sys} ∪ ⋃{resolve-dependencies(d) | d ∈ direct_deps(sys)}

where:
  DependencyGraph = DirectedAcyclicGraph
  LoadOrder = [SystemName]  ; Topologically sorted

  valid_load_order(order, deps) ≡
    ∀ i < j : ¬depends-on(order[i], order[j])
```

## Informal Explanation

Dependency resolution must correctly handle complex dependency relationships:

1. **Transitive Closure**: Find all direct and indirect dependencies
2. **Topological Order**: Determine correct load order
3. **Completeness**: No missing dependencies
4. **Cycle Detection**: Identify circular dependencies
5. **Minimality**: Include only necessary dependencies

This ensures:
- Systems load in correct order
- Dependencies available when needed
- No "undefined function" errors
- Efficient loading (no redundant systems)

## Rationale

Correct dependency resolution is critical because:
- Common Lisp systems often have deep dependency trees
- Wrong load order causes cryptic errors
- Missing dependencies break functionality
- Circular dependencies cause infinite loops
- Users expect automatic dependency handling

Incorrect resolution would cause:
- "Function not found" errors
- "Package not found" errors
- Load failures
- Compilation errors
- Infinite loading loops

Key correctness requirements:
- Compute complete transitive closure
- Generate valid topological ordering
- Detect and report cycles
- Handle diamond dependencies correctly
- Respect conditional dependencies

## Counterexample Shape

If this property is violated:

**Missing Transitive Dependency**:
```lisp
;; System A depends on B
;; System B depends on C

;; resolve-dependencies("A", transitive=true) returns:
["B"]  ; VIOLATION! Missing C

;; Loading fails:
(quickload "A")
; Error: Package C not found (needed by B)
```

**Wrong Load Order**:
```lisp
;; System A depends on B, C
;; System B depends on C
;; Correct order: [C, B, A]

;; compute-load-order returns:
["A", "B", "C"]  ; VIOLATION! Wrong order

;; Loading fails:
(asdf:load-system "A")
; Error: Function from C not found (needed by B)
```

**Circular Dependency Not Detected**:
```lisp
;; System A depends on B
;; System B depends on A (circular!)

;; resolve-dependencies("A") returns:
["B", "A", "B", "A", ...]  ; VIOLATION! Infinite loop

;; Or doesn't detect cycle:
(quickload "A")
; Hangs forever or stack overflow
```

**Diamond Dependency Duplication**:
```lisp
;; System A depends on B, C
;; Both B and C depend on D
;;
;;     A
;;    / \
;;   B   C
;;    \ /
;;     D

;; resolve-dependencies("A") returns:
["B", "D", "C", "D"]  ; VIOLATION! D appears twice

;; Should be: ["D", "B", "C", "A"] or ["D", "C", "B", "A"]
```

**Incomplete Closure**:
```lisp
;; Hunchentoot depends on:
;;   usocket, chunga, bordeaux-threads, ...
;; Chunga depends on:
;;   trivial-gray-streams

;; resolve-dependencies("hunchentoot") returns:
["usocket", "chunga", "bordeaux-threads"]
; VIOLATION! Missing trivial-gray-streams (transitive via chunga)

(quickload "hunchentoot")
; Error: TRIVIAL-GRAY-STREAMS package not found
```

**Ignoring Conditional Dependencies**:
```lisp
;; System has platform-specific dependency:
(defsystem "my-system"
  :depends-on ("alexandria"
               #+sbcl "sb-posix"
               #+ccl "ccl-specific"))

;; On SBCL, resolve-dependencies("my-system") returns:
["alexandria"]
; VIOLATION! Missing sb-posix on SBCL

(asdf:load-system "my-system")
; Error: SB-POSIX not found
```

**Wrong Order with Multiple Dependencies**:
```lisp
;; A depends on B, C, D
;; B depends on C
;; C depends on D
;; Correct: [D, C, B, A]

;; compute-load-order returns:
["D", "B", "C", "A"]  ; VIOLATION! B before C
; But B needs C!

(asdf:load-system "A")
; Error: Package C not found (needed by B)
```

## Verification Approach

**Generator**: Use known systems with complex dependency graphs

**Assertion**:
```lisp
(defun verify-dependency-resolution (system-name)
  ;; Get transitive dependencies
  (let* ((transitive (system-dependencies system-name :transitive t))
         (direct (system-dependencies system-name :transitive nil)))

    (and
      ;; All direct deps in transitive
      (subsetp direct transitive :test #'string-equal)

      ;; Transitive closure is complete
      (every (lambda (dep)
               (let ((dep-deps (system-dependencies dep :transitive t)))
                 (subsetp dep-deps transitive :test #'string-equal)))
             direct)

      ;; No duplicates in transitive list
      (= (length transitive)
         (length (remove-duplicates transitive :test #'string-equal)))

      ;; Can compute valid load order
      (valid-load-order-p system-name transitive)

      ;; Actually loading works
      (handler-case
          (progn
            (quickload system-name)
            t)
        (error () nil)))))

(defun valid-load-order-p (system-name deps)
  "Verify load order satisfies dependency constraints."
  (let ((order (compute-load-order system-name deps)))
    ;; For each dependency relationship, verify order
    (every (lambda (sys)
             (let ((sys-deps (system-dependencies sys :transitive nil))
                   (sys-pos (position sys order :test #'string-equal)))
               ;; All deps appear before sys in order
               (every (lambda (dep)
                        (let ((dep-pos (position dep order :test #'string-equal)))
                          (< dep-pos sys-pos)))
                      sys-deps)))
           (cons system-name deps))))
```

**Property Test Strategy**:

1. **Simple Chain**:
   ```lisp
   ;; A → B → C
   ;; Verify: C, B, A order
   (let ((deps (system-dependencies "A" :transitive t)))
     (assert (equal deps '("B" "C")))
     (assert (valid-load-order-p "A" deps)))
   ```

2. **Diamond Dependency**:
   ```lisp
   ;; A depends on B, C
   ;; B, C both depend on D
   (let ((deps (system-dependencies "A" :transitive t)))
     ;; Should include D only once
     (assert (= 1 (count "D" deps :test #'string-equal)))
     ;; D should load before B and C
     (let ((order (compute-load-order "A" deps)))
       (assert (< (position "D" order)
                  (position "B" order)))
       (assert (< (position "D" order)
                  (position "C" order)))))
   ```

3. **Complex Graph (Hunchentoot)**:
   ```lisp
   ;; Hunchentoot has 10+ dependencies
   (let ((deps (system-dependencies "hunchentoot" :transitive t)))
     ;; Should include all transitive deps
     (assert (member "usocket" deps :test #'string-equal))
     (assert (member "bordeaux-threads" deps :test #'string-equal))
     (assert (member "flexi-streams" deps :test #'string-equal))

     ;; Should be able to load
     (handler-case
         (progn (quickload "hunchentoot") t)
       (error () nil)))
   ```

4. **Transitive Closure Completeness**:
   ```lisp
   ;; For each dep, verify its deps are included
   (defun check-closure (system)
     (let ((trans (system-dependencies system :transitive t)))
       (every (lambda (dep)
                (let ((dep-deps (system-dependencies dep :transitive t)))
                  (subsetp dep-deps trans :test #'string-equal)))
              trans)))

   (assert (check-closure "drakma"))
   ```

5. **Cycle Detection**:
   ```lisp
   ;; If circular dependency exists, should detect
   ;; (Hard to test without creating circular system)
   (handler-case
       (system-dependencies "circular-system-a")
     (circular-dependency-error (e)
       ;; Should report cycle
       (assert (search "circular" (princ-to-string e)))))
   ```

6. **Load Order Verification**:
   ```lisp
   ;; Verify computed order actually works
   (defun test-load-order (system-name)
     (let* ((deps (system-dependencies system-name :transitive t))
            (order (compute-load-order system-name deps)))
       ;; Try loading in computed order
       (dolist (sys order)
         (asdf:load-system sys))
       t))

   (assert (test-load-order "hunchentoot"))
   ```

7. **Minimal Closure**:
   ```lisp
   ;; Verify no extra systems included
   (let ((deps (system-dependencies "alexandria" :transitive t)))
     ;; Alexandria has few/no dependencies
     (assert (< (length deps) 5)))
   ```

**Edge Cases**:

- Systems with no dependencies
- Systems with only optional dependencies
- Conditional dependencies (platform-specific)
- Systems with :defsystem-depends-on
- Very deep dependency chains (20+ levels)
- Wide dependency trees (50+ dependencies)
- Missing optional dependencies
- Preloaded systems (already in image)

**Implementation Requirements**:

```lisp
;; Transitive closure computation
(defun compute-transitive-dependencies (system-name)
  "Compute complete transitive closure of dependencies."
  (let ((visited (make-hash-table :test #'equal))
        (result '()))
    (labels ((visit (sys)
               (unless (gethash sys visited)
                 (setf (gethash sys visited) t)
                 (let ((deps (asdf:system-depends-on
                              (asdf:find-system sys))))
                   (dolist (dep deps)
                     (pushnew dep result :test #'string-equal)
                     (visit dep))))))
      (visit system-name))
    (nreverse result)))

;; Topological sort for load order
(defun compute-load-order (system-name deps)
  "Compute valid topological ordering of dependencies."
  (let ((in-degree (make-hash-table :test #'equal))
        (adjacency (make-hash-table :test #'equal))
        (result '())
        (queue '()))

    ;; Build graph
    (dolist (sys (cons system-name deps))
      (let ((sys-deps (asdf:system-depends-on
                        (asdf:find-system sys))))
        (setf (gethash sys in-degree) (length sys-deps))
        (setf (gethash sys adjacency) sys-deps)))

    ;; Kahn's algorithm
    (maphash (lambda (sys degree)
               (when (zerop degree)
                 (push sys queue)))
             in-degree)

    (loop while queue do
      (let ((sys (pop queue)))
        (push sys result)
        (dolist (neighbor (gethash sys adjacency))
          (decf (gethash neighbor in-degree))
          (when (zerop (gethash neighbor in-degree))
            (push neighbor queue)))))

    (nreverse result)))
```

**Shrinking**: Find minimal dependency graph that resolves incorrectly

## Related Properties

- **system-dependency-accuracy**: Dependencies correctly identified
- **quickload-safety**: Dependencies from trusted sources
- **load-idempotency**: Repeated loads handle deps correctly
- **system-state-tracking**: Loaded deps tracked in session
