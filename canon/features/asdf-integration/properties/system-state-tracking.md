---
type: property
name: system-state-tracking
version: 0.1.0
feature: asdf-integration
covers:
  - contracts/quickload-tool
  - contracts/describe-system-tool
  - contracts/list-local-systems-tool
---

# System State Tracking Property

## Statement

**For all** system loading operations,
**the session** MUST accurately track which systems are loaded, their versions, and their load status at all times.

## Formal Expression

```
∀ session ∈ Sessions, ∀ operation ∈ SystemOperations :
  let state₀ = session.loaded_systems
  let result = execute(operation)
  let state₁ = session.loaded_systems
  then:
    1. ∀ sys ∈ successfully_loaded(result) :
         sys ∈ state₁ ∧ sys ∉ state₀
    2. ∀ sys ∈ failed_to_load(result) :
         sys ∉ state₁
    3. list-definitions() reflects loaded system definitions
    4. describe-system(sys) shows correct load status
    5. Reloading same system updates state correctly

where:
  SessionState = {
    loaded_systems: Map<SystemName, SystemInfo>,
    load_time: Map<SystemName, Timestamp>,
    dependencies: Map<SystemName, [SystemName]>
  }

  SystemInfo = {
    name: String,
    version: String,
    loaded: Boolean,
    source: Path,
    load_time: Timestamp
  }
```

## Informal Explanation

The session must maintain accurate state about loaded systems:

1. **Load Tracking**: Record when systems are loaded successfully
2. **Failure Tracking**: Don't mark failed loads as loaded
3. **Version Tracking**: Track which version of each system is loaded
4. **Dependency Tracking**: Know which dependencies were loaded
5. **Definition Visibility**: Loaded system definitions appear in list-definitions
6. **Status Queries**: describe-system reports correct load status

This allows Claude to:
- Know what functionality is available
- Avoid redundant loads
- Understand system state
- Debug load failures
- Track session evolution

## Rationale

Accurate state tracking is essential for:
- Knowing what code is available in the session
- Avoiding duplicate system loads
- Understanding feature availability
- Debugging "undefined function" errors
- Planning subsequent loads

Inaccurate state tracking would:
- Cause confusion about available features
- Lead to redundant loads
- Make debugging impossible
- Report wrong system status
- Lose track of what's in the image

Key tracking requirements:
- State updates must be atomic (all or nothing)
- Failed loads must not pollute state
- System versions must be tracked
- Load times must be recorded
- Dependencies must be tracked

## Counterexample Shape

If this property is violated:

**Loaded System Not Tracked**:
```lisp
;; Before load
(system-loaded-p "alexandria") → NIL

;; Load system successfully
(quickload "alexandria")
; => "Loaded system: ALEXANDRIA (version 1.4)"

;; After load - VIOLATION! Should show as loaded
(system-loaded-p "alexandria") → NIL  ; WRONG!
```

**Failed Load Marked as Loaded**:
```lisp
;; Try to load non-existent system
(handler-case
    (quickload "nonexistent-system-xyz")
  (error (e) :failed))

;; VIOLATION! Failed system appears as loaded
(system-loaded-p "nonexistent-system-xyz") → T  ; WRONG!
```

**Wrong Version Tracked**:
```lisp
;; Load specific version
(quickload "drakma")  ; Loads version 2.0.8

;; VIOLATION! Wrong version tracked
(describe-system "drakma")
; => version: "2.0.7"  ; WRONG! Should be 2.0.8
```

**Definitions Not Visible**:
```lisp
;; Load system with known functions
(quickload "alexandria")

;; VIOLATION! Loaded definitions not visible
(list-definitions :package "ALEXANDRIA")
; => []  ; WRONG! Should show copy-sequence, hash-table-keys, etc.
```

**Dependency Not Tracked**:
```lisp
;; Load system with dependencies
(quickload "hunchentoot")
; Dependencies: usocket, bordeaux-threads, chunga, ...

;; VIOLATION! Dependencies not tracked
(system-dependencies "hunchentoot")
; => []  ; WRONG! Should show all loaded dependencies
```

**Reload Not Tracked**:
```lisp
;; Load system
(quickload "alexandria")
(system-load-time "alexandria") → "2026-01-26T10:30:00"

;; Reload system (force recompile)
(sleep 5)
(asdf:load-system "alexandria" :force t)

;; VIOLATION! Load time not updated
(system-load-time "alexandria") → "2026-01-26T10:30:00"  ; WRONG!
; Should be ~10:30:05
```

**Stale State After Reset**:
```lisp
;; Load some systems
(quickload "alexandria")
(quickload "cl-ppcre")

;; Reset session
(reset-session)

;; VIOLATION! Old state persists
(list-loaded-systems) → ["alexandria", "cl-ppcre"]  ; WRONG!
; Should be empty after reset
```

## Verification Approach

**Generator**: Perform various system operations and verify state consistency

**Assertion**:
```lisp
(defun verify-state-tracking ()
  ;; Start with clean state
  (reset-session)
  (assert (null (list-loaded-systems)))

  ;; Load system
  (quickload "alexandria")
  (assert (member "alexandria" (list-loaded-systems) :test #'string-equal))

  ;; Verify version tracked
  (let ((info (describe-system "alexandria")))
    (assert (not (null (system-version info)))))

  ;; Verify definitions visible
  (let ((defs (list-definitions :package "ALEXANDRIA")))
    (assert (> (length defs) 0))
    (assert (member "COPY-SEQUENCE" defs :test #'string-equal)))

  ;; Verify load time tracked
  (let ((load-time (system-load-time "alexandria")))
    (assert (not (null load-time)))
    (assert (timestamp-recent-p load-time)))

  ;; Load with dependencies
  (quickload "drakma")
  (let ((loaded (list-loaded-systems)))
    (assert (member "drakma" loaded :test #'string-equal))
    (assert (member "usocket" loaded :test #'string-equal))
    (assert (member "flexi-streams" loaded :test #'string-equal)))

  ;; Verify failed load doesn't pollute state
  (let ((before (list-loaded-systems)))
    (handler-case
        (quickload "nonexistent-xyz-123")
      (error (e) :failed))
    (let ((after (list-loaded-systems)))
      (assert (equal before after))
      (assert (not (member "nonexistent-xyz-123" after :test #'string-equal)))))

  ;; Verify reload updates state
  (let ((time1 (system-load-time "alexandria")))
    (sleep 1)
    (asdf:load-system "alexandria" :force t)
    (let ((time2 (system-load-time "alexandria")))
      (assert (timestamp> time2 time1))))

  t)
```

**Property Test Strategy**:

1. **Basic Load Tracking**:
   ```lisp
   ;; Verify successful load is tracked
   (reset-session)
   (quickload "alexandria")
   (assert (system-loaded-p "alexandria"))
   ```

2. **Failed Load Tracking**:
   ```lisp
   ;; Verify failed load is NOT tracked
   (handler-case
       (quickload "does-not-exist-xyz")
     (error () nil))
   (assert (not (system-loaded-p "does-not-exist-xyz")))
   ```

3. **Version Tracking**:
   ```lisp
   ;; Verify version is tracked
   (quickload "cl-ppcre")
   (let ((info (describe-system "cl-ppcre")))
     (assert (string-match "\\d+\\.\\d+" (system-version info))))
   ```

4. **Definition Visibility**:
   ```lisp
   ;; Verify loaded definitions are visible
   (quickload "alexandria")
   (assert (fboundp 'alexandria:hash-table-keys))
   (assert (member "HASH-TABLE-KEYS"
                   (list-definitions :package "ALEXANDRIA")
                   :test #'string-equal))
   ```

5. **Dependency Tracking**:
   ```lisp
   ;; Verify dependencies are tracked
   (quickload "hunchentoot")
   (let ((loaded (list-loaded-systems)))
     (assert (every (lambda (dep)
                      (member dep loaded :test #'string-equal))
                    '("usocket" "bordeaux-threads" "chunga"))))
   ```

6. **Load Time Tracking**:
   ```lisp
   ;; Verify load time is recorded
   (let ((before (get-universal-time)))
     (quickload "alexandria")
     (let ((after (get-universal-time))
           (load-time (system-load-time "alexandria")))
       (assert (<= before load-time after))))
   ```

7. **Reload Tracking**:
   ```lisp
   ;; Verify reload updates state
   (quickload "alexandria")
   (let ((time1 (system-load-time "alexandria")))
     (sleep 2)
     (asdf:load-system "alexandria" :force t)
     (let ((time2 (system-load-time "alexandria")))
       (assert (> time2 time1))))
   ```

8. **Reset State**:
   ```lisp
   ;; Verify reset clears state
   (quickload "alexandria")
   (quickload "cl-ppcre")
   (reset-session)
   (assert (null (list-loaded-systems)))
   ```

**Edge Cases**:

- Loading system that's already loaded
- Loading system with circular dependencies
- Partial load failure (some deps loaded, main system fails)
- Loading multiple versions of same system
- Systems loaded outside tool (via REPL)
- Systems unloaded (not typical, but possible)
- Very large systems with many dependencies

**Implementation Requirements**:

```lisp
;; Track loaded systems
(defvar *loaded-systems* (make-hash-table :test #'equal))

(defstruct system-info
  name
  version
  loaded-at
  dependencies
  source-file)

(defun track-system-load (system-name)
  "Record that a system has been loaded."
  (let* ((system (asdf:find-system system-name))
         (version (asdf:component-version system))
         (deps (asdf:system-depends-on system))
         (source (asdf:system-source-file system)))
    (setf (gethash (string-upcase system-name) *loaded-systems*)
          (make-system-info
            :name system-name
            :version version
            :loaded-at (get-universal-time)
            :dependencies deps
            :source-file source))))

(defun system-loaded-p (system-name)
  "Check if system is loaded in current session."
  (gethash (string-upcase system-name) *loaded-systems*))

(defun list-loaded-systems ()
  "List all loaded systems."
  (hash-table-keys *loaded-systems*))
```

**Shrinking**: Find minimal sequence of operations that causes state inconsistency

## Related Properties

- **load-idempotency**: Re-loading same system preserves state correctly
- **system-dependency-accuracy**: Dependencies correctly tracked
- **dependency-resolution-correctness**: Transitive deps tracked
- **session-state-persistence**: State survives across evaluations
