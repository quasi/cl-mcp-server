---
type: property
name: profiling-overhead-bounded
version: 0.1.0
feature: profiling
covers:
  - contracts/profile-code-tool
  - contracts/profile-functions-tool
---

# Profiling Overhead Bounded Property

## Statement

**For all** profiled code execution,
**the profiling overhead** MUST be bounded and documented, ensuring measurements reflect actual performance characteristics.

## Formal Expression

```
∀ code ∈ ExecutableCode, ∀ mode ∈ ProfilingMode :
  let time_profiled = execution_time(profile(code, mode))
  let time_unprofiled = execution_time(code)
  let overhead = (time_profiled - time_unprofiled) / time_unprofiled
  then:
    overhead ≤ MaxOverhead(mode)

where:
  MaxOverhead(statistical-cpu) = 0.15     (15% max)
  MaxOverhead(statistical-time) = 0.15    (15% max)
  MaxOverhead(statistical-alloc) = 0.20   (20% max)
  MaxOverhead(deterministic) = 1.00       (100% max, 2x slowdown)
```

## Informal Explanation

Profiling must have predictable, bounded overhead:

1. **Statistical profiling**: Low overhead (5-15%)
2. **Deterministic profiling**: Higher but bounded overhead (~30-100%)
3. **Documented limits**: Known overhead per profiling mode
4. **No pathological cases**: Overhead doesn't scale with code size
5. **Measurement validity**: Overhead doesn't distort relative timings

This ensures profiling data remains meaningful and trustworthy.

## Rationale

Bounded overhead is essential for:
- Usable profiling tools (not impractically slow)
- Valid measurements (overhead doesn't dominate)
- Production profiling (acceptable impact)
- Trust in results (overhead predictable)

Unbounded overhead would:
- Make profiling impractical
- Distort performance characteristics
- Hide real bottlenecks under profiling cost
- Create misleading measurements

Trade-offs by profiling mode:
- **Statistical**: Low overhead, approximate results
- **Deterministic**: High overhead, exact counts
- Choice depends on use case

## Verification Approach

**Property Test Strategy**:

1. **Statistical CPU Overhead**:
   ```lisp
   ;; Measure baseline
   (let ((baseline (measure-execution-time
                     "(loop repeat 10000000 sum 1)")))
     ;; Measure with profiling
     (let ((profiled (profile-code
                       "(loop repeat 10000000 sum 1)"
                       :mode :cpu)))
       ;; Overhead should be < 15%
       (let ((overhead (/ (- (execution-time profiled) baseline)
                         baseline)))
         (assert (< overhead 0.15)))))
   ```

2. **Statistical Time Overhead**:
   ```lisp
   ;; I/O bound code
   (let ((baseline (measure-execution-time
                     "(sleep 0.5)")))
     (let ((profiled (profile-code "(sleep 0.5)" :mode :time)))
       ;; Time mode overhead should also be < 15%
       (let ((overhead (/ (- (execution-time profiled) baseline)
                         baseline)))
         (assert (< overhead 0.15)))))
   ```

3. **Allocation Profiling Overhead**:
   ```lisp
   ;; Allocation-heavy code
   (let ((baseline (measure-execution-time
                     "(loop repeat 100000 collect (cons 1 2))")))
     (let ((profiled (profile-code
                       "(loop repeat 100000 collect (cons 1 2))"
                       :mode :alloc)))
       ;; Alloc mode has slightly higher overhead < 20%
       (let ((overhead (/ (- (execution-time profiled) baseline)
                         baseline)))
         (assert (< overhead 0.20)))))
   ```

4. **Deterministic Overhead**:
   ```lisp
   ;; Simple function profiling
   (let ((baseline (measure-execution-time
                     "(defun test () (+ 1 2))
                      (loop repeat 1000000 do (test))")))
     (progn
       (sb-profile:profile test)
       (let ((profiled (measure-execution-time
                         "(loop repeat 1000000 do (test))")))
         ;; Deterministic can be up to 2x slower
         (let ((overhead (/ (- profiled baseline) baseline)))
           (assert (< overhead 1.0))))))
   ```

5. **Overhead Scaling**:
   ```lisp
   ;; Overhead shouldn't scale pathologically with code size
   (let ((small-overhead (profile-overhead
                           "(loop repeat 1000 sum 1)"))
         (large-overhead (profile-overhead
                           "(loop repeat 100000000 sum 1)")))
     ;; Both should have similar relative overhead
     (assert (< (abs (- small-overhead large-overhead)) 0.05)))
   ```

6. **Sample Interval Impact**:
   ```lisp
   ;; Higher sample rate = higher overhead
   (let ((low-rate (profile-code code
                     :sample-interval 0.1   ; 10 samples/sec
                     :max-samples 100))
         (high-rate (profile-code code
                      :sample-interval 0.001  ; 1000 samples/sec
                      :max-samples 100)))
     ;; High rate should have measurably higher overhead
     ;; but still bounded
     (assert (> (execution-time high-rate)
                (execution-time low-rate)))
     (assert (< (/ (execution-time high-rate)
                   (execution-time low-rate))
                1.5)))
   ```

7. **Overhead Reporting**:
   ```lisp
   ;; Tool should report profiling overhead
   (let ((profile (profile-code "(+ 1 2 3)")))
     ;; Metadata should include overhead estimate
     (assert (has-key profile :overhead-estimate))
     (assert (numberp (get-field profile :overhead-estimate))))
   ```

**Comparative Verification**:

```lisp
(defun verify-overhead-bounded (code)
  ;; Run without profiling (3 times for stability)
  (let ((baseline-times
          (loop repeat 3 collect
            (measure-execution-time code))))
    (let ((baseline (median baseline-times)))

      ;; Run with profiling (3 times)
      (let ((profiled-times
              (loop repeat 3 collect
                (execution-time (profile-code code)))))
        (let ((profiled (median profiled-times)))

          ;; Calculate overhead
          (let ((overhead (/ (- profiled baseline) baseline)))
            ;; Should be < 15% for statistical profiling
            (< overhead 0.15)))))))
```

**Edge Cases**:

- Very fast code (<10ms) - overhead may be proportionally higher
- System calls - profiling can't add overhead to kernel time
- Multi-threaded code - overhead per thread
- Recursive functions - overhead per call
- Inlined functions - no overhead if inlined away

## Counterexample Shape

**Excessive Statistical Overhead**:
```lisp
;; Simple computation
(let ((baseline 1.0))  ; 1 second unprofiled

  (profile-code "(loop repeat 100000000 sum 1)")
  ;; Takes: 5.0 seconds  ; VIOLATION!
  ;; 400% overhead exceeds 15% limit
  ```

**Unbounded Deterministic Overhead**:
```lisp
(let ((baseline 0.5))  ; 0.5 seconds unprofiled

  ;; Profile with sb-profile
  (profile-functions :start :functions '(test))
  (run-code)
  ;; Takes: 2.0 seconds  ; VIOLATION!
  ;; 300% overhead exceeds 100% limit
  ```

**Scaling Overhead**:
```lisp
;; Small workload
(profile-code "(loop repeat 1000 sum 1)")
;; Overhead: 10%

;; Large workload (100x larger)
(profile-code "(loop repeat 100000 sum 1)")
;; Overhead: 45%  ; VIOLATION!
;; Overhead shouldn't scale with workload size
```

**Undocumented Overhead**:
```lisp
(profile-code code)
;; Returns: { samples: [...], report: "...", result: 42 }
;; VIOLATION! Missing overhead information
;; Should include: { ..., overhead_pct: 8.5, ... }
```

**Mode Overhead Violation**:
```lisp
;; Alloc mode
(let ((baseline 1.0))
  (profile-code allocation-heavy-code :mode :alloc)
  ;; Takes: 1.5 seconds  ; 50% overhead
  ;; VIOLATION! Exceeds 20% limit for alloc mode
  ```

## Implementation Notes

### Measuring Overhead

```lisp
(defun measure-profiling-overhead (code mode)
  ;; Baseline measurement
  (let ((baseline-start (get-internal-run-time)))
    (eval (read-from-string code))
    (let ((baseline-end (get-internal-run-time)))
      (let ((baseline-ms (/ (* (- baseline-end baseline-start) 1000.0)
                           internal-time-units-per-second)))

        ;; Profiled measurement
        (let ((prof-start (get-internal-run-time)))
          (profile-code code :mode mode)
          (let ((prof-end (get-internal-run-time)))
            (let ((profiled-ms (/ (* (- prof-end prof-start) 1000.0)
                                 internal-time-units-per-second)))

              ;; Calculate overhead
              (/ (- profiled-ms baseline-ms) baseline-ms))))))))
```

### Overhead Sources

1. **Sample collection**: Timer interrupts and stack capture
2. **Stack walking**: Unwinding call stack at each sample
3. **Data storage**: Recording samples in memory
4. **Instrumentation**: (Deterministic only) Function wrapping
5. **Reporting**: Generating profile report

### Minimizing Overhead

```lisp
;; Use coarser sample interval for lower overhead
(profile-code code
  :sample-interval 0.02   ; 50 samples/sec instead of 100
  :max-samples 500)       ; Fewer total samples

;; For deterministic, profile only key functions
(profile-functions :start
  :functions '(hot-spot-1 hot-spot-2)
  ;; Not entire package
  )
```

## Related Properties

- **sampling-accuracy**: Low overhead ensures valid measurements
- **timing-accuracy**: Overhead doesn't distort timing data
- **profiling-safety**: Overhead doesn't cause crashes or errors
