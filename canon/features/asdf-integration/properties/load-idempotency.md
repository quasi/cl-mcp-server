---
type: property
name: load-idempotency
version: 0.1.0
feature: asdf-integration
covers:
  - contracts/quickload-tool
  - contracts/load-file-tool
---

# Load Idempotency Property

## Statement

**For all** system and file loading operations,
**loading the same system or file multiple times** MUST be safe and produce consistent results without duplicate side effects.

## Formal Expression

```
∀ system ∈ Systems, ∀ state₀ ∈ SessionState :
  let (result₁, state₁) = load(system, state₀)
  let (result₂, state₂) = load(system, state₁)
  then:
    1. No duplicate definitions:
         functions(state₂) = functions(state₁)
    2. No duplicate warnings:
         warnings(result₂) ⊆ warnings(result₁)
    3. Version remains consistent:
         version(system, state₂) = version(system, state₁)
    4. No duplicate side effects:
         side_effects(result₂) = ∅ (no file writes, prints, etc.)
    5. Performance: second load faster (uses cached FASLs)

where:
  LoadResult = {
    success: Boolean,
    warnings: [Warning],
    definitions: [Definition],
    side_effects: [Effect],
    load_time: Duration
  }
```

## Informal Explanation

Loading the same system or file multiple times must be safe:

1. **No Redefinition Warnings**: Second load doesn't warn about existing definitions
2. **No Duplicate Effects**: Side effects (if any) don't run twice
3. **Consistent State**: Repeated loads produce same final state
4. **Performance**: Subsequent loads use cached compiled files
5. **Version Stability**: Same version loaded each time (unless forced)

This allows Claude to:
- Safely retry failed operations
- Reload systems after changes
- Use load operations speculatively
- Avoid worrying about load order

## Rationale

Idempotent loading is essential because:
- Load operations may need to be retried
- Users may load same system multiple times
- Dependencies may cause system to be loaded multiple times
- Development workflow involves frequent reloading
- Error recovery may require re-loading

Non-idempotent loading would cause:
- Duplicate definition warnings (noise)
- Repeated side effects (broken invariants)
- Performance degradation (recompiling unnecessarily)
- State inconsistencies
- Confusion about system status

Key idempotency requirements:
- Definitions can be safely redefined
- Compiled files are cached and reused
- No-op when system already loaded (unless :force)
- Side effects protected by run-once guards
- Consistent behavior across reloads

## Counterexample Shape

If this property is violated:

**Duplicate Definition Warnings**:
```lisp
;; First load
(quickload "alexandria")
; => "Loaded system: ALEXANDRIA (version 1.4)"

;; Second load - VIOLATION! Shouldn't warn
(quickload "alexandria")
; => "WARNING: Redefining COPY-SEQUENCE
;     WARNING: Redefining HASH-TABLE-KEYS
;     ..."  ; VIOLATION! Should be silent
```

**Duplicate Side Effects**:
```lisp
;; System has initialization code
(defvar *init-count* 0)

(asdf:defsystem "side-effect-system"
  :components ((:file "init"))
  :perform (load-op :after (o c)
    (incf *init-count*)))

;; First load
(asdf:load-system "side-effect-system")
*init-count* → 1

;; Second load - VIOLATION! Side effect runs again
(asdf:load-system "side-effect-system")
*init-count* → 2  ; WRONG! Should still be 1
```

**Recompilation Without Force**:
```lisp
;; First load compiles and loads
(time (quickload "cl-ppcre"))
; => 2.3 seconds (compilation time)

;; Second load - VIOLATION! Shouldn't recompile
(time (quickload "cl-ppcre"))
; => 2.2 seconds  ; WRONG! Should be ~0.1s (cached)
```

**Version Inconsistency**:
```lisp
;; First load
(quickload "some-system")
(system-version "some-system") → "1.2.3"

;; Second load without force - VIOLATION! Version changed
(quickload "some-system")
(system-version "some-system") → "1.2.4"  ; WRONG!
; Version shouldn't change without explicit update
```

**Stale Function Definitions**:
```lisp
;; Load system with function foo
(quickload "my-system")
(fboundp 'my-system:foo) → T

;; Reload after source changed (added function bar)
(asdf:load-system "my-system" :force t)

;; VIOLATION! New definitions not visible
(fboundp 'my-system:bar) → NIL  ; WRONG! Should be T
```

**File Load Not Idempotent**:
```lisp
;; File contains: (defvar *counter* 0) (incf *counter*)
(load-file "counter.lisp")
*counter* → 1

;; Load again - VIOLATION! Side effect repeated
(load-file "counter.lisp")
*counter* → 2  ; WRONG! Unless this is desired behavior
```

## Verification Approach

**Generator**: Load systems and files multiple times, verify consistency

**Assertion**:
```lisp
(defun verify-load-idempotency (system-name)
  ;; First load
  (let* ((start1 (get-internal-real-time))
         (result1 (quickload system-name))
         (time1 (- (get-internal-real-time) start1))
         (functions1 (list-definitions))
         (version1 (system-version system-name)))

    ;; Second load (should be cached)
    (let* ((start2 (get-internal-real-time))
           (result2 (quickload system-name))
           (time2 (- (get-internal-real-time) start2))
           (functions2 (list-definitions))
           (version2 (system-version system-name)))

      (and
        ;; Same functions defined
        (set-equal functions1 functions2 :test #'string-equal)

        ;; Same version
        (equal version1 version2)

        ;; Second load faster (using cache)
        (< time2 (* 0.5 time1))  ; At least 2x faster

        ;; No duplicate warnings in second load
        (or (null (warnings result2))
            (< (length (warnings result2))
               (length (warnings result1))))

        ;; Both loads successful
        (and (success-p result1) (success-p result2))))))
```

**Property Test Strategy**:

1. **Basic Idempotency**:
   ```lisp
   ;; Load twice, verify same result
   (quickload "alexandria")
   (let ((defs1 (list-definitions :package "ALEXANDRIA")))
     (quickload "alexandria")
     (let ((defs2 (list-definitions :package "ALEXANDRIA")))
       (assert (equal defs1 defs2))))
   ```

2. **Performance Check**:
   ```lisp
   ;; Second load should be faster
   (let ((time1 (time (quickload "drakma"))))
     (let ((time2 (time (quickload "drakma"))))
       (assert (< time2 (* 0.5 time1)))))
   ```

3. **No Redefinition Warnings**:
   ```lisp
   ;; Capture warnings
   (quickload "cl-ppcre")  ; First load may warn
   (let ((warnings (capture-warnings
                     (lambda () (quickload "cl-ppcre")))))
     (assert (null warnings)))  ; Second load silent
   ```

4. **Side Effect Protection**:
   ```lisp
   ;; Create system with side effect
   (let ((counter 0))
     (defun increment-once ()
       (when (zerop counter)
         (incf counter)))

     ;; Load twice
     (load-file "side-effect.lisp")
     (load-file "side-effect.lisp")

     ;; Verify side effect ran once
     (assert (= counter 1)))
   ```

5. **Version Consistency**:
   ```lisp
   ;; Verify version doesn't change
   (quickload "alexandria")
   (let ((v1 (system-version "alexandria")))
     (quickload "alexandria")
     (let ((v2 (system-version "alexandria")))
       (assert (equal v1 v2))))
   ```

6. **Force Reload Behavior**:
   ```lisp
   ;; Force should recompile
   (quickload "cl-ppcre")
   (let ((time-normal (time (quickload "cl-ppcre")))
         (time-force (time (asdf:load-system "cl-ppcre" :force t))))
     (assert (< time-normal time-force)))  ; Forced takes longer
   ```

7. **File Load Idempotency**:
   ```lisp
   ;; Load file twice
   (load-file "/path/to/definitions.lisp")
   (let ((defs1 (list-definitions)))
     (load-file "/path/to/definitions.lisp")
     (let ((defs2 (list-definitions)))
       (assert (equal defs1 defs2))))
   ```

8. **Multiple Sequential Loads**:
   ```lisp
   ;; Load many times
   (dotimes (i 10)
     (quickload "alexandria"))
   ;; Should still be consistent
   (assert (system-loaded-p "alexandria"))
   (assert (fboundp 'alexandria:hash-table-keys))
   ```

**Edge Cases**:

- Loading while source files are being modified
- Loading with compilation errors
- Loading with missing dependencies
- Loading after system unload (rare)
- Loading with :force flag
- Loading with :verbose flag
- Concurrent loads (from multiple threads)
- Loading precompiled vs source

**Implementation Requirements**:

```lisp
;; Idempotent load wrapper
(defun idempotent-quickload (system-name &key verbose)
  ;; Check if already loaded
  (let ((already-loaded (system-loaded-p system-name)))
    (if already-loaded
        ;; Quick return if already loaded
        (format nil "System ~A already loaded (version ~A)~%"
                system-name
                (system-version system-name))
        ;; Load for first time
        (progn
          (ql:quickload system-name :silent (not verbose))
          (track-system-load system-name)))))

;; ASDF provides idempotency:
;; - operate method checks if operation needed
;; - Uses file timestamps to detect changes
;; - Reuses FASLs when source unchanged
;; - component-operation-time tracks what's done

;; Side effects should be guarded:
(defvar *initialized* nil)

(defun initialize-once ()
  (unless *initialized*
    (perform-side-effect)
    (setf *initialized* t)))
```

**Shrinking**: Find minimal load sequence that violates idempotency

## Related Properties

- **system-state-tracking**: Load state tracked correctly across reloads
- **compilation-safety**: Compilation doesn't cause unexpected side effects
- **session-isolation**: Multiple sessions don't interfere with caching
- **load-file-isolation**: File loads don't corrupt session state
