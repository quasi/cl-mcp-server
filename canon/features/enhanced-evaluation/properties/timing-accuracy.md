---
type: property
name: timing-accuracy
version: 0.1.0
feature: enhanced-evaluation
covers:
  - contracts/time-execution-tool
---

# Timing Accuracy Property

## Statement

**For all** code executed via `time-execution`,
**the reported timing measurements** MUST accurately reflect actual execution costs within measurement precision limits.

## Formal Expression

```
∀ code ∈ ExecutableCode, ∀ ε ∈ MeasurementError :
  let timing = time-execution(code).timing
  let actual = measure-actual-cost(code)
  then:
    |timing.real_time_ms - actual.real_time_ms| ≤ ε_real
    |timing.run_time_ms - actual.run_time_ms| ≤ ε_run
    |timing.bytes_allocated - actual.bytes_allocated| ≤ ε_alloc

where:
  ε_real ≤ 0.1ms  (measurement precision)
  ε_run ≤ 0.1ms   (run-time precision)
  ε_alloc ≤ 1024 bytes (allocation tracking precision)
```

## Informal Explanation

Timing measurements must be accurate and trustworthy:

1. **Real time reflects wall-clock**: Includes I/O, sleeps, blocking
2. **Run time reflects CPU**: User-mode execution time only
3. **GC time separated**: Garbage collection tracked separately
4. **Allocation tracked**: Heap allocation measured correctly
5. **Precision documented**: Known measurement limits stated

This ensures Claude can trust timing data for performance analysis.

## Rationale

Accurate timing is essential for:
- Identifying performance bottlenecks
- Comparing algorithm implementations
- Detecting regressions
- Understanding resource costs

Inaccurate timing would:
- Lead to wrong optimization decisions
- Hide expensive operations
- Make profiling useless

## Verification Approach

**Property Test Strategy**:

1. **Sleep Accuracy Test**:
   ```lisp
   ;; Verify real-time tracks sleep
   (let ((timing (time-execution "(sleep 0.1)")))
     (assert (>= (timing-real-time-ms timing) 100.0))
     (assert (<  (timing-real-time-ms timing) 110.0)))
   ```

2. **CPU-Bound Accuracy**:
   ```lisp
   ;; Run known computation
   (let ((timing (time-execution "(loop repeat 1000000 sum 1)")))
     ;; Run time should be non-trivial
     (assert (> (timing-run-time-ms timing) 1.0)))
   ```

3. **Allocation Tracking**:
   ```lisp
   ;; Known allocation pattern
   (let ((timing (time-execution "(make-list 10000)")))
     ;; Should allocate ~160KB (cons cells)
     (assert (> (timing-bytes-allocated timing) 100000)))
   ```

4. **Zero Allocation Path**:
   ```lisp
   ;; Simple arithmetic
   (let ((timing (time-execution "(+ 1 2 3)")))
     ;; Should allocate nothing
     (assert (= (timing-bytes-allocated timing) 0)))
   ```

5. **GC Triggering**:
   ```lisp
   ;; Force GC
   (let ((timing (time-execution
                   "(progn (gc :full t)
                          (loop repeat 1000000 collect (cons 1 2)))")))
     ;; GC time should be non-zero
     (assert (> (timing-gc-time-ms timing) 0)))
   ```

**Comparative Verification**:

Compare timing from tool vs. direct measurement:

```lisp
(defun verify-timing-accuracy ()
  ;; Measure directly
  (let ((start (get-internal-real-time)))
    (sleep 0.05)
    (let* ((end (get-internal-real-time))
           (direct-ms (/ (* (- end start) 1000.0)
                        internal-time-units-per-second)))

      ;; Measure via tool
      (let ((tool-ms (timing-real-time-ms
                      (time-execution "(sleep 0.05)"))))

        ;; Should match within tolerance
        (< (abs (- tool-ms direct-ms)) 1.0)))))
```

**Edge Cases**:

- Very fast code (<0.1ms) - measurement overhead significant
- GC during execution - properly attributed
- Errors during execution - timing still captured
- Multi-threaded code - timing may be complex

## Counterexample Shape

**Inaccurate Real Time**:
```lisp
(time-execution "(sleep 1)")
;; Returns: { real_time_ms: 10 }  ; VIOLATION!
;; Should be ~1000ms
```

**Missing Allocation**:
```lisp
(time-execution "(make-list 100000)")
;; Returns: { bytes_allocated: 0 }  ; VIOLATION!
;; Should show significant allocation
```

**Wrong GC Time**:
```lisp
(time-execution "(progn (gc :full t) (+ 1 2))")
;; Returns: { gc_time_ms: 0 }  ; VIOLATION!
;; Should show GC time
```
