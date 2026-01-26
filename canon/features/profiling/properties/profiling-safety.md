---
type: property
name: profiling-safety
version: 0.1.0
feature: profiling
covers:
  - contracts/profile-code-tool
  - contracts/profile-functions-tool
  - contracts/allocation-profile-tool
  - contracts/memory-report-tool
---

# Profiling Safety Property

## Statement

**For all** profiling operations,
**profiling never crashes or corrupts program state**, ensuring safe observation of executing code.

## Formal Expression

```
∀ code ∈ ExecutableCode, ∀ mode ∈ ProfilingMode, ∀ state₀ ∈ ProgramState :
  let (result, state₁) = profile(code, mode, state₀)
  then:
    1. result ∈ (ProfileReport ∪ ErrorReport)
       (Either success or structured error, never crash)
    2. state₁.definitions = state₀.definitions
       (Profiling doesn't modify definitions)
    3. state₁.is_consistent
       (Program state remains valid)
    4. result.code_result = execute(code, state₀).result
       (Code result unchanged by profiling)

where:
  ProgramState = {
    definitions: Set[Definition],
    package_state: PackageState,
    dynamic_bindings: Bindings,
    is_consistent: Boolean
  }
```

## Informal Explanation

Profiling must be completely safe:

1. **No crashes**: Profiling never causes segfaults or aborts
2. **State preservation**: Program state unchanged by profiling
3. **Error handling**: Errors during profiling are caught and reported
4. **Result integrity**: Profiled code produces same result as unprofiled
5. **Thread safety**: Profiling safe in multi-threaded contexts

This ensures Claude can profile any code without risk.

## Rationale

Profiling safety is essential for:
- Confidence in using profiling tools
- Production debugging (profiling live systems)
- Exploring unfamiliar code
- Automated profiling in tests

Unsafe profiling would:
- Crash programs under investigation
- Corrupt application state
- Make profiling unusable in production
- Limit profiling to safe/known code only

Key safety requirements:
- Profiler state isolated from program state
- Signal handlers properly installed/removed
- Memory allocation tracked safely
- Profiler never modifies program behavior

## Verification Approach

**Property Test Strategy**:

1. **State Preservation Test**:
   ```lisp
   ;; Define functions before profiling
   (defun my-function () 42)
   (defvar *my-var* 100)

   ;; Profile some code
   (profile-code "(loop repeat 100000 sum 1)")

   ;; State should be unchanged
   (assert (fboundp 'my-function))
   (assert (= (my-function) 42))
   (assert (boundp '*my-var*))
   (assert (= *my-var* 100))
   ```

2. **Error Recovery Test**:
   ```lisp
   ;; Profile code that errors
   (let ((result (profile-code "(/ 1 0)")))
     ;; Should return error report, not crash
     (assert (profile-result-p result))
     (assert (profile-has-error result))
     ;; Can continue profiling after error
     (let ((result2 (profile-code "(+ 1 2 3)")))
       (assert (profile-success-p result2))
       (assert (= (profile-code-result result2) 6))))
   ```

3. **Result Integrity Test**:
   ```lisp
   ;; Compare results with and without profiling
   (let ((unprofiled (execute-code code)))
     (let ((profiled (profile-code-result (profile-code code))))
       ;; Results should be identical
       (assert (equal unprofiled profiled))))
   ```

4. **Recursive Profiling Test**:
   ```lisp
   ;; Start profiling, then profile again
   (profile-functions :start :functions '(test))
   (let ((result (profile-code "(test)")))
     ;; Should handle nested profiling safely
     (assert (profile-result-p result)))
   (profile-functions :stop)
   ```

5. **Signal Handler Safety**:
   ```lisp
   ;; Profile code with custom signal handler
   (handler-case
     (profile-code
       "(error \"Test error\")")
     (error (e)
       ;; Should catch error safely
       (assert t)))

   ;; Profiler signal handlers should be cleaned up
   (assert (= (length (sb-sys:list-all-timers)) 0))
   ```

6. **Memory Safety Test**:
   ```lisp
   ;; Profile allocation-heavy code
   (let ((result (allocation-profile
                   "(loop repeat 10000000 collect (cons 1 2))")))
     ;; Should complete without memory corruption
     (assert (profile-result-p result))
     ;; Memory report should still work
     (assert (memory-report-p (memory-report))))
   ```

7. **Package Safety Test**:
   ```lisp
   ;; Profile in different package
   (let ((*package* (find-package :cl-user)))
     (profile-code "(defun test () 42)" :package "CL-USER")
     ;; Package state should be intact
     (assert (eq *package* (find-package :cl-user)))
     (assert (fboundp 'test)))
   ```

8. **Stack Overflow Safety**:
   ```lisp
   ;; Profile code that would overflow stack
   (let ((result (profile-code
                   "(defun infinite () (infinite)) (infinite)")))
     ;; Should catch stack overflow, not crash
     (assert (profile-has-error result))
     ;; Can continue after stack overflow
     (assert (profile-result-p (profile-code "(+ 1 2)"))))
   ```

9. **Thread Safety Test**:
   ```lisp
   ;; Profile in threaded context (if supported)
   (let ((thread (sb-thread:make-thread
                   (lambda ()
                     (profile-code "(loop repeat 1000000 sum 1)")))))
     (sb-thread:join-thread thread)
     ;; Should complete without crash
     (assert t))
   ```

10. **Cleanup Test**:
    ```lisp
    ;; Start profiling
    (profile-functions :start :functions '(test))

    ;; Check profiling state
    (assert (profiling-active-p))

    ;; Reset session
    (reset-session)

    ;; Profiling should be cleaned up
    (assert (not (profiling-active-p)))
    ```

**Property Test**:

```lisp
(defun verify-profiling-safety (code)
  "Verify profiling doesn't crash or corrupt state"
  ;; Capture initial state
  (let ((initial-fns (list-all-functions))
        (initial-vars (list-all-variables))
        (initial-pkgs (list-all-packages)))

    ;; Profile code (may error, should not crash)
    (handler-case
      (let ((profile-result (profile-code code)))

        ;; Verify state unchanged
        (and (equal initial-fns (list-all-functions))
             (equal initial-vars (list-all-variables))
             (equal initial-pkgs (list-all-packages))

             ;; Verify profiler cleaned up
             (not (profiling-active-p))

             ;; Result is valid
             (profile-result-p profile-result)))

      (error (e)
        ;; Even on error, state should be intact
        (and (equal initial-fns (list-all-functions))
             (equal initial-vars (list-all-variables))
             (equal initial-pkgs (list-all-packages)))))))
```

**Edge Cases**:

- Stack overflow - should be caught
- Infinite loops - timeout protection
- Memory exhaustion - graceful failure
- Signal conflicts - proper cleanup
- Nested profiling - handled safely
- Compiler errors during profiling - caught
- Foreign function calls - profiled safely

## Counterexample Shape

**Crash on Profiling**:
```lisp
(profile-code "(loop repeat 100000000 sum 1)")

;; Result: Segmentation fault (core dumped)
;; VIOLATION! Should never crash
```

**State Corruption**:
```lisp
(defun my-func () 42)

(profile-code "(loop repeat 1000 sum 1)")

(my-func)
;; Error: UNDEFINED-FUNCTION MY-FUNC
;; VIOLATION! Profiling corrupted function definitions
```

**Unhandled Error**:
```lisp
(profile-code "(/ 1 0)")

;; Error: Division by zero (unhandled)
;; Program terminates
;; VIOLATION! Error should be caught and reported
```

**Wrong Result**:
```lisp
(execute-code "(+ 1 2 3)")
;; => 6

(profile-code-result (profile-code "(+ 1 2 3)"))
;; => 8
;; VIOLATION! Profiling changed code result
```

**Profiler Not Cleaned Up**:
```lisp
(profile-functions :start :functions '(test))
(profile-code "(test)")

;; Later...
(profiling-active-p)
;; => T
;; VIOLATION! Profiler should be cleaned up after profile-code
```

**Memory Corruption**:
```lisp
(profile-code "(make-list 1000000)")

(memory-report)
;; Result: Segmentation fault
;; VIOLATION! Profiling corrupted memory management
```

## Implementation Notes

### Error Handling

```lisp
(defun safe-profile-code (code &key mode)
  (handler-case
    (handler-bind
      ((error
        (lambda (condition)
          ;; Capture error but allow profiling to complete
          (setf *profiling-error* condition))))

      ;; Wrap profiling in unwind-protect
      (unwind-protect
        (progn
          (start-profiling mode)
          (execute-code code))

        ;; Always cleanup, even on error
        (stop-profiling)))

    (error (e)
      ;; Return error report, never crash
      (make-error-report
        :error-type (type-of e)
        :message (princ-to-string e)))))
```

### State Isolation

```lisp
(defun profile-with-isolation (code)
  ;; Profiler state separate from program state
  (let ((*profiler-enabled* t)
        (*profile-samples* (make-array 1000))
        (*profile-index* 0))

    ;; Program state unchanged
    (execute-code code)))
```

### Signal Handler Safety

```lisp
(defun install-profiling-timer (interval)
  ;; Install signal handler
  (sb-sys:enable-interrupt
    sb-unix:sigprof
    #'profiler-signal-handler)

  ;; Set timer
  (setf *profiling-timer*
    (sb-ext:make-timer #'collect-sample
                       :thread sb-thread:*current-thread*)))

(defun cleanup-profiling-timer ()
  ;; Remove timer
  (when *profiling-timer*
    (sb-ext:unschedule-timer *profiling-timer*)
    (setf *profiling-timer* nil))

  ;; Restore signal handler
  (sb-sys:enable-interrupt
    sb-unix:sigprof
    :default))
```

### Memory Safety

```lisp
;; Pre-allocate profiler buffers
(defvar *sample-buffer* (make-array 10000))
(defvar *sample-index* 0)

(defun collect-sample ()
  ;; Don't allocate during profiling
  (when (< *sample-index* (length *sample-buffer*))
    (setf (aref *sample-buffer* *sample-index*)
          (capture-stack-sample))
    (incf *sample-index*)))
```

### Cleanup

```lisp
(defun ensure-profiling-cleanup ()
  (unwind-protect
    (run-profiling)
    ;; Always cleanup
    (progn
      (cleanup-profiling-timer)
      (reset-profiling-state)
      (restore-signal-handlers))))
```

## Related Properties

- **evaluation-isolation**: Profiling doesn't affect other evaluations
- **condition-handling**: Errors during profiling are caught
- **state-persistence**: Program state preserved across profiling
- **timeout-protection**: Infinite loops don't hang profiler
