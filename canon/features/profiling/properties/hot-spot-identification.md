---
type: property
name: hot-spot-identification
version: 0.1.0
feature: profiling
covers:
  - contracts/profile-code-tool
  - contracts/profile-functions-tool
---

# Hot Spot Identification Property

## Statement

**For all** profiled code,
**the most expensive functions** (hot spots) MUST be correctly identified and ranked by actual execution cost.

## Formal Expression

```
∀ code ∈ ExecutableCode, ∀ profile ∈ ProfileResult :
  let hotspots = extract_hotspots(profile, threshold=5%)
  let actual = measure_actual_costs(code)
  then:
    ∀ i, j : i < j ⇒ cost(hotspots[i]) ≥ cost(hotspots[j])
    ∧
    ∀ f ∈ hotspots : actual_cost(f) ≥ threshold × total_cost

where:
  cost(f) = self_time(f)  (time in function excluding callees)
  actual_cost(f) = measured_execution_time(f)
  threshold = 0.05  (5% of total execution time)
```

## Informal Explanation

Profiling must correctly identify and rank expensive functions:

1. **Hot spot detection**: Functions consuming >5% of time identified
2. **Correct ranking**: Functions ordered by actual cost
3. **Cost attribution**: Time correctly attributed to functions
4. **Exclusive time**: Self-time excludes callees
5. **Inclusive time**: Includes time in called functions

This allows Claude to focus optimization effort on actual bottlenecks.

## Rationale

Accurate hot spot identification is essential for:
- Effective performance optimization
- Efficient use of optimization time
- Understanding performance characteristics
- Making data-driven decisions

Incorrect identification would:
- Waste effort optimizing wrong code
- Miss actual bottlenecks
- Lead to ineffective optimization
- Undermine trust in profiling

Key identification requirements:
- Top hot spots are actually expensive
- Ranking reflects actual cost
- Minor functions excluded from top spots
- Both self and inclusive time reported

## Verification Approach

**Property Test Strategy**:

1. **Single Hot Spot Test**:
   ```lisp
   ;; One clear hot spot
   (let ((profile (profile-code
                    "(defun fast () (+ 1 2))
                     (defun slow () (loop repeat 100000000 sum 1))
                     (fast) (slow)")))
     ;; SLOW should be top hot spot
     (assert (string= (first-hot-spot profile) "SLOW"))
     ;; SLOW should dominate (>90% of time)
     (assert (> (self-time-pct profile "SLOW") 90.0))
     ;; FAST should not be in top 3
     (assert (not (member "FAST" (top-n-hot-spots profile 3)))))
   ```

2. **Multiple Hot Spots**:
   ```lisp
   ;; Multiple significant functions
   (let ((profile (profile-code
                    "(defun a () (loop repeat 60000000 sum 1))
                     (defun b () (loop repeat 30000000 sum 2))
                     (defun c () (loop repeat 10000000 sum 3))
                     (a) (b) (c)")))
     ;; Should be ranked A > B > C
     (let ((top-3 (top-n-hot-spots profile 3)))
       (assert (string= (nth 0 top-3) "A"))
       (assert (string= (nth 1 top-3) "B"))
       (assert (string= (nth 2 top-3) "C")))
     ;; Proportions should be approximately 6:3:1
     (let ((pct-a (self-time-pct profile "A"))
           (pct-b (self-time-pct profile "B"))
           (pct-c (self-time-pct profile "C")))
       (assert (> pct-a pct-b pct-c))))
   ```

3. **Caller vs Callee Attribution**:
   ```lisp
   ;; Expensive callee, cheap caller
   (let ((profile (profile-code
                    "(defun expensive () (loop repeat 100000000 sum 1))
                     (defun caller () (expensive))
                     (caller)")))
     ;; EXPENSIVE should have high self-time
     (assert (> (self-time-pct profile "EXPENSIVE") 80.0))
     ;; CALLER should have low self-time
     (assert (< (self-time-pct profile "CALLER") 5.0))
     ;; But CALLER should have high inclusive time
     (assert (> (inclusive-time-pct profile "CALLER") 90.0)))
   ```

4. **Recursive Function**:
   ```lisp
   ;; Recursive hot spot
   (let ((profile (profile-code
                    "(defun fib (n)
                       (if (<= n 1) n
                           (+ (fib (- n 1)) (fib (- n 2)))))
                     (fib 30)")))
     ;; FIB should be top hot spot
     (assert (string= (first-hot-spot profile) "FIB"))
     ;; Should show recursion in call graph
     (when (graph-available profile)
       (assert (shows-recursion profile "FIB"))))
   ```

5. **Threshold Test**:
   ```lisp
   ;; Many small functions + one hot spot
   (let ((profile (profile-code
                    "(defun hot () (loop repeat 100000000 sum 1))
                     (defun cold-1 () (+ 1 2))
                     (defun cold-2 () (* 3 4))
                     (defun cold-3 () (- 5 6))
                     (hot) (cold-1) (cold-2) (cold-3)")))
     ;; Only HOT should be in hot spots (>5% threshold)
     (let ((hotspots (functions-above-threshold profile 5.0)))
       (assert (= (length hotspots) 1))
       (assert (string= (first hotspots) "HOT"))))
   ```

6. **Inlined Functions**:
   ```lisp
   ;; Simple function might be inlined
   (let ((profile (profile-code
                    "(declaim (inline tiny))
                     (defun tiny (x) (+ x 1))
                     (defun caller ()
                       (loop repeat 100000000 sum (tiny 1)))
                     (caller)")))
     ;; CALLER should be hot spot
     (assert (member "CALLER" (top-n-hot-spots profile 3)))
     ;; TINY might not appear (inlined)
     ;; This is correct behavior
     )
   ```

7. **System Functions**:
   ```lisp
   ;; Code using system functions
   (let ((profile (profile-code
                    "(loop repeat 10000000 collect (random 100))")))
     ;; RANDOM should appear as hot spot
     (assert (member "RANDOM" (top-n-hot-spots profile 5))))
   ```

**Comparative Verification**:

```lisp
(defun verify-hotspot-ranking (code)
  ;; Profile with statistical profiling
  (let ((stat-profile (profile-code code :max-samples 1000)))

    ;; Profile with deterministic profiling
    (let ((functions (extract-function-names code)))
      (dolist (fn functions)
        (sb-profile:profile fn))
      (eval (read-from-string code))
      (let ((det-report (sb-profile:report)))

        ;; Extract top 3 from both
        (let ((stat-top (top-n-hot-spots stat-profile 3))
              (det-top (extract-top-functions det-report 3)))

          ;; Top function should match
          (string= (first stat-top) (first det-top)))))))
```

**Edge Cases**:

- Tail-recursive functions - proper attribution
- Macro-generated code - attribute to macro or expansion
- Generic function dispatch - include dispatch overhead
- Very short functions - may be inlined
- Profiler itself - should not appear as hot spot

## Counterexample Shape

**Wrong Hot Spot**:
```lisp
(profile-code "(defun fast () (+ 1 2))
               (defun slow () (loop repeat 100000000 sum 1))
               (fast) (slow)")

;; Returns: Top hot spot is FAST
;; VIOLATION! SLOW is clearly the hot spot
```

**Wrong Ranking**:
```lisp
(profile-code "(defun a () (loop repeat 80000000 sum 1))
               (defun b () (loop repeat 20000000 sum 2))
               (a) (b)")

;; Returns:
;;   1. B (70%)
;;   2. A (25%)
;; VIOLATION! Ranking is backwards
```

**Wrong Attribution**:
```lisp
(profile-code "(defun expensive () (loop repeat 100000000 sum 1))
               (defun wrapper () (expensive))
               (wrapper)")

;; Returns:
;;   WRAPPER: 95% self-time
;;   EXPENSIVE: 5% self-time
;; VIOLATION! Time attributed to caller not callee
```

**Missing Hot Spot**:
```lisp
(profile-code "(defun hot () (loop repeat 100000000 sum 1))
               (hot)")

;; Returns: Top hot spots: [LOOP, SUM, +]
;; VIOLATION! HOT function should be identified, not just internals
```

**False Hot Spot**:
```lisp
(profile-code "(defun trivial () 42)
               (defun actual-work () (loop repeat 100000000 sum 1))
               (trivial) (actual-work)")

;; Returns: Hot spots include TRIVIAL (35%)
;; VIOLATION! TRIVIAL is not expensive
```

## Implementation Notes

### Hot Spot Extraction

```lisp
(defun extract-hot-spots (profile &key (threshold 5.0))
  "Extract functions consuming >threshold% of execution time"
  (let ((total-samples (profile-total-samples profile)))
    (remove-if
      (lambda (entry)
        (< (* 100.0 (/ (entry-samples entry) total-samples))
           threshold))
      (profile-functions profile))))
```

### Ranking by Cost

```lisp
(defun rank-by-self-time (profile)
  "Sort functions by self-time (exclusive)"
  (sort (copy-list (profile-functions profile))
        #'>
        :key (lambda (entry)
               (entry-self-time entry))))

(defun rank-by-inclusive-time (profile)
  "Sort functions by inclusive-time (including callees)"
  (sort (copy-list (profile-functions profile))
        #'>
        :key (lambda (entry)
               (entry-inclusive-time entry))))
```

### Self vs Inclusive Time

```lisp
;; Self time: time in function itself
(defun calculate-self-time (function call-graph)
  (- (total-time function)
     (sum (mapcar #'total-time (callees function call-graph)))))

;; Inclusive time: time in function + callees
(defun calculate-inclusive-time (function call-graph)
  (+ (self-time function)
     (sum (mapcar (lambda (callee)
                   (calculate-inclusive-time callee call-graph))
                 (callees function call-graph)))))
```

### Report Format

```lisp
;; Hot spot report format
(defstruct hot-spot-report
  (function-name nil :type string)
  (self-time-ms 0.0 :type float)
  (self-time-pct 0.0 :type float)
  (inclusive-time-ms 0.0 :type float)
  (inclusive-time-pct 0.0 :type float)
  (sample-count 0 :type integer)
  (call-count 0 :type (or integer null)))
```

## Related Properties

- **sampling-accuracy**: Accurate samples → correct hot spot identification
- **profiling-overhead-bounded**: Low overhead → valid measurements
- **memory-tracking-accuracy**: Allocation hot spots correctly identified
