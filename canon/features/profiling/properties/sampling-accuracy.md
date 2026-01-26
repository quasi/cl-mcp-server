---
type: property
name: sampling-accuracy
version: 0.1.0
feature: profiling
covers:
  - contracts/profile-code-tool
---

# Sampling Accuracy Property

## Statement

**For all** code profiled via `profile-code` with statistical sampling,
**the reported hot spots** MUST accurately represent actual execution patterns within statistical confidence bounds.

## Formal Expression

```
∀ code ∈ ProfilableCode, ∀ samples ∈ Samples :
  let profile = profile-code(code, samples)
  let actual = measure-actual-execution(code)
  then:
    ∀ function ∈ TopHotSpots(profile, threshold=5%) :
      function ∈ ActualHotSpots(actual, threshold=5%) ∨
      StatisticalNoise(function, samples) < α

where:
  α = 0.05  (95% confidence level)
  TopHotSpots(p, t) = {f | self_time(f) ≥ t × total_time}
  ActualHotSpots(a, t) = {f | measured_time(f) ≥ t × total_time}
  StatisticalNoise(f, n) = 1/√n  (standard error)
```

## Informal Explanation

Statistical profiling must provide trustworthy hot spot identification:

1. **Representative sampling**: Samples capture actual execution patterns
2. **Hot spot accuracy**: Functions reported as expensive actually are expensive
3. **No false positives**: Functions not hot aren't reported as hot
4. **Statistical validity**: Sample count sufficient for confidence
5. **Consistent results**: Multiple runs identify same hot spots

This ensures Claude can trust profiling data for optimization decisions.

## Rationale

Accurate sampling is essential for:
- Identifying real performance bottlenecks
- Making correct optimization decisions
- Avoiding wasted optimization effort
- Building confidence in profiling tools

Inaccurate sampling would:
- Point to wrong optimization targets
- Hide actual bottlenecks
- Lead to ineffective performance work
- Undermine trust in profiling

Key sampling requirements:
- Sufficient samples for statistical significance
- Unbiased sampling (no systematic skew)
- Low perturbation of actual execution
- Accurate time attribution

## Verification Approach

**Property Test Strategy**:

1. **Known Hot Spot Test**:
   ```lisp
   ;; Create known CPU hot spot
   (let ((profile (profile-code
                    "(defun expensive () (loop repeat 100000000 sum 1))
                     (expensive)"
                    :mode :cpu)))
     ;; EXPENSIVE should be top hot spot
     (assert (string= (first-hot-spot profile) "EXPENSIVE"))
     (assert (>= (hot-spot-percentage profile "EXPENSIVE") 80.0)))
   ```

2. **Multiple Hot Spots**:
   ```lisp
   ;; Code with two hot spots
   (let ((profile (profile-code
                    "(defun hot-a () (loop repeat 50000000 sum 1))
                     (defun hot-b () (loop repeat 30000000 sum 2))
                     (hot-a) (hot-b)"
                    :max-samples 1000)))
     ;; Both should appear in top spots
     (assert (member "HOT-A" (top-n-hot-spots profile 3)))
     (assert (member "HOT-B" (top-n-hot-spots profile 3)))
     ;; Relative proportions should be ~5:3
     (let ((ratio (/ (hot-spot-percentage profile "HOT-A")
                    (hot-spot-percentage profile "HOT-B"))))
       (assert (< 1.5 ratio 2.0))))
   ```

3. **Cold Function Test**:
   ```lisp
   ;; Fast function shouldn't appear as hot spot
   (let ((profile (profile-code
                    "(defun hot () (loop repeat 100000000 sum 1))
                     (defun cold () (+ 1 2))
                     (hot) (cold)")))
     ;; COLD should not be in top 3
     (assert (not (member "COLD" (top-n-hot-spots profile 3)))))
   ```

4. **Statistical Consistency**:
   ```lisp
   ;; Run same code multiple times
   (let* ((code "(defun test () (loop repeat 50000000 sum 1)) (test)")
          (profiles (loop repeat 5 collect
                      (profile-code code :max-samples 500))))
     ;; All runs should identify TEST as top hot spot
     (assert (every (lambda (p)
                      (string= (first-hot-spot p) "TEST"))
                    profiles))
     ;; Percentages should be similar (within 10%)
     (let ((percentages (mapcar (lambda (p)
                                  (hot-spot-percentage p "TEST"))
                                profiles)))
       (assert (< (standard-deviation percentages) 10.0))))
   ```

5. **Sample Count Impact**:
   ```lisp
   ;; More samples → more accurate
   (let ((low-sample (profile-code long-code :max-samples 100))
         (high-sample (profile-code long-code :max-samples 1000)))
     ;; High sample count should have similar but more stable results
     (assert (hot-spots-similar low-sample high-sample)))
   ```

6. **Minimal Sample Warning**:
   ```lisp
   ;; Code too fast for profiling
   (let ((profile (profile-code "(+ 1 2 3)")))
     ;; Should warn about insufficient samples
     (assert (or (zerop (sample-count profile))
                 (< (sample-count profile) 10)))
     (assert (contains-warning profile "too quickly")))
   ```

**Comparative Verification**:

Compare statistical profile with deterministic profile:

```lisp
(defun verify-sampling-accuracy (code)
  ;; Run statistical profiling
  (let ((stat-profile (profile-code code :mode :cpu :max-samples 1000)))

    ;; Run deterministic profiling for ground truth
    (sb-profile:reset)
    (sb-profile:profile test-function)
    (eval (read-from-string code))
    (let ((det-report (sb-profile:report)))

      ;; Top 3 functions should overlap significantly
      (let ((stat-top (top-n-hot-spots stat-profile 3))
            (det-top (extract-top-functions det-report 3)))
        ;; At least 2/3 should match
        (>= (count-if (lambda (f) (member f det-top))
                      stat-top)
            2)))))
```

**Edge Cases**:

- Very fast code (<10ms) - insufficient samples
- Code with I/O waits - CPU vs time mode differences
- Recursive functions - correct attribution
- Inlined functions - may not appear separately
- System functions - may be filtered from report

## Counterexample Shape

**False Hot Spot**:
```lisp
(profile-code "(defun fast () (+ 1 2))
               (defun slow () (loop repeat 100000000 sum 1))
               (fast) (slow)")

;; Returns: Top hot spot is FAST  ; VIOLATION!
;; Should be SLOW (100M iterations vs simple addition)
```

**Missing Hot Spot**:
```lisp
(profile-code "(defun expensive ()
                 (loop repeat 100000000 sum 1))
               (expensive)")

;; Returns: Top hot spots: [EVAL, LOOP-SUM, +]  ; VIOLATION!
;; Should show EXPENSIVE as primary hot spot
```

**Wrong Proportions**:
```lisp
(profile-code "(defun a () (loop repeat 80000000 sum 1))
               (defun b () (loop repeat 20000000 sum 1))
               (a) (b)")

;; Returns:
;;   A: 30% of samples
;;   B: 70% of samples  ; VIOLATION!
;; Should be approximately 4:1 ratio (A:B)
```

**Inconsistent Results**:
```lisp
;; Run 1
(profile-code long-code) => Top: FOO (80%)

;; Run 2 (same code)
(profile-code long-code) => Top: BAR (75%)  ; VIOLATION!
;; Same code should identify same hot spots
```

## Related Properties

- **profiling-overhead-bounded**: Profiling doesn't distort measurements
- **hot-spot-identification**: Most expensive functions correctly ranked
- **sampling-statistical-validity**: Sufficient samples for confidence
