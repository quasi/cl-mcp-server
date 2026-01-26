---
type: property
name: memory-tracking-accuracy
version: 0.1.0
feature: profiling
covers:
  - contracts/allocation-profile-tool
  - contracts/memory-report-tool
---

# Memory Tracking Accuracy Property

## Statement

**For all** memory profiling operations,
**allocation and GC statistics** MUST accurately reflect actual memory usage within measurement precision limits.

## Formal Expression

```
∀ code ∈ ExecutableCode :
  let mem_report = memory-report()
  let alloc_profile = allocation-profile(code)
  let actual = measure-actual-memory(code)
  then:
    |alloc_profile.bytes_allocated - actual.bytes_allocated| ≤ ε_alloc
    ∧
    |mem_report.dynamic_space - actual.heap_used| ≤ ε_heap
    ∧
    mem_report.gc_runs = actual.gc_runs
    ∧
    |mem_report.gc_time_ms - actual.gc_time_ms| ≤ ε_gc

where:
  ε_alloc ≤ 8192 bytes  (one page tolerance)
  ε_heap ≤ 65536 bytes  (heap measurement precision)
  ε_gc ≤ 1.0 ms         (GC time precision)
```

## Informal Explanation

Memory tracking must provide accurate statistics:

1. **Allocation tracking**: Bytes allocated measured correctly
2. **Heap usage**: Dynamic space usage accurate
3. **GC statistics**: Collection counts and times correct
4. **Allocation sources**: Functions allocating memory identified
5. **Generation stats**: Generational GC data accurate

This allows Claude to understand memory behavior and optimize allocation.

## Rationale

Accurate memory tracking is essential for:
- Identifying memory leaks
- Reducing allocation overhead
- Understanding GC behavior
- Optimizing memory usage

Inaccurate tracking would:
- Hide allocation hot spots
- Misrepresent GC impact
- Lead to ineffective optimization
- Waste memory investigation effort

Key tracking requirements:
- Allocation attributed to correct functions
- GC statistics match actual collections
- Heap usage reflects live objects
- Generation data shows object ages

## Verification Approach

**Property Test Strategy**:

1. **Known Allocation Test**:
   ```lisp
   ;; Allocate known amount
   (let ((profile (allocation-profile
                    "(make-list 10000)")))  ; ~160KB cons cells
     ;; Should report approximately 160KB
     (let ((allocated (bytes-allocated profile)))
       (assert (> allocated 150000))
       (assert (< allocated 170000))))
   ```

2. **Zero Allocation Test**:
   ```lisp
   ;; Code with no allocation
   (let ((profile (allocation-profile "(+ 1 2 3)")))
     ;; Should report zero or near-zero allocation
     (assert (< (bytes-allocated profile) 1000)))
   ```

3. **Allocation Attribution**:
   ```lisp
   ;; Multiple allocating functions
   (let ((profile (allocation-profile
                    "(defun alloc-a () (make-list 5000))
                     (defun alloc-b () (make-list 3000))
                     (alloc-a) (alloc-b)")))
     ;; Both should appear in allocation report
     (assert (member "ALLOC-A" (allocation-hot-spots profile)))
     (assert (member "ALLOC-B" (allocation-hot-spots profile)))
     ;; Relative proportions should be ~5:3
     (let ((ratio (/ (function-allocated profile "ALLOC-A")
                    (function-allocated profile "ALLOC-B"))))
       (assert (< 1.5 ratio 2.0))))
   ```

4. **GC Triggering**:
   ```lisp
   ;; Allocate enough to trigger GC
   (let ((mem-before (memory-report))
         (profile (allocation-profile
                    "(loop repeat 1000000 collect (cons 1 2))"))
         (mem-after (memory-report)))
     ;; Should show GC occurred
     (assert (> (gc-runs mem-after)
                (gc-runs mem-before)))
     ;; GC time should be non-zero
     (assert (> (gc-time-ms mem-after)
                (gc-time-ms mem-before))))
   ```

5. **Heap Usage Accuracy**:
   ```lisp
   ;; Check heap before and after allocation
   (let ((mem-before (memory-report)))
     (progn
       (gc :full t)  ; Clean slate
       (let ((big-data (make-array 1000000 :initial-element 42)))
         (let ((mem-after (memory-report)))
           ;; Dynamic space should increase by ~4MB (plus overhead)
           (let ((increase (- (dynamic-space mem-after)
                             (dynamic-space mem-before))))
             (assert (> increase 4000000))
             (assert (< increase 5000000)))))))
   ```

6. **Generation Statistics**:
   ```lisp
   ;; Check generational GC stats
   (let ((mem (memory-report)))
     ;; Should have generation data
     (assert (has-field mem :gen-0-collections))
     (assert (has-field mem :gen-1-collections))
     ;; Gen 0 collections >= Gen 1 collections
     (assert (>= (gen-0-collections mem)
                 (gen-1-collections mem))))
   ```

7. **GC Time Tracking**:
   ```lisp
   ;; Force GC and measure time
   (let ((start (get-internal-run-time)))
     (gc :full t)
     (let ((end (get-internal-run-time)))
       (let ((measured-gc-ms
               (/ (* (- end start) 1000.0)
                  internal-time-units-per-second)))
         ;; Memory report should show similar GC time
         (let ((mem (memory-report)))
           ;; Total GC time should include this collection
           (assert (>= (total-gc-time-ms mem) measured-gc-ms))))))
   ```

8. **Allocation by Type**:
   ```lisp
   ;; Different allocation types
   (let ((cons-profile (allocation-profile "(make-list 10000)"))
         (array-profile (allocation-profile
                          "(make-array 10000 :initial-element 0)")))
     ;; Both should show allocation
     (assert (> (bytes-allocated cons-profile) 0))
     (assert (> (bytes-allocated array-profile) 0))
     ;; Array should allocate more (contiguous vs cons cells)
     (assert (> (bytes-allocated array-profile)
                (bytes-allocated cons-profile))))
   ```

**Comparative Verification**:

```lisp
(defun verify-allocation-tracking (code)
  ;; Use time-execution for ground truth
  (let ((timing (time-execution code)))
    (let ((time-allocated (timing-bytes-allocated timing)))

      ;; Use allocation-profile
      (let ((profile (allocation-profile code)))
        (let ((profile-allocated (bytes-allocated profile)))

          ;; Should match within 10%
          (< (abs (- profile-allocated time-allocated))
             (* 0.1 time-allocated)))))))
```

**Edge Cases**:

- Very small allocations (<1KB) - may be in registers
- Stack allocation - not tracked in heap
- GC during profiling - affects measurements
- Object promotion - moves between generations
- Compiler optimizations - may eliminate allocation

## Counterexample Shape

**Allocation Underreporting**:
```lisp
(allocation-profile "(make-list 10000)")

;; Returns: { bytes_allocated: 1024 }
;; VIOLATION! Should be ~160KB for 10000 cons cells
```

**Wrong GC Count**:
```lisp
;; Before: 10 GC runs
(progn
  (loop repeat 1000000 collect (cons 1 2))
  (let ((mem (memory-report))))

;; Returns: { gc_runs: 10 }
;; VIOLATION! Should show increased GC count
```

**Wrong Attribution**:
```lisp
(allocation-profile
  "(defun alloc-heavy () (make-list 100000))
   (defun alloc-light () (cons 1 2))
   (alloc-heavy) (alloc-light)")

;; Returns:
;;   ALLOC-LIGHT: 95% of allocation
;;   ALLOC-HEAVY: 5% of allocation
;; VIOLATION! Attribution is backwards
```

**Missing Heap Usage**:
```lisp
(memory-report)

;; Returns: { status: "ok" }
;; VIOLATION! Missing dynamic_space, heap usage data
```

**Wrong GC Time**:
```lisp
;; Force expensive GC
(gc :full t)  ; Takes ~500ms

(memory-report)
;; Returns: { total_gc_time_ms: 10 }
;; VIOLATION! GC time far too low
```

**Generation Inconsistency**:
```lisp
(memory-report)

;; Returns:
;;   gen_0_collections: 5
;;   gen_1_collections: 20
;; VIOLATION! Gen 1 can't have more collections than Gen 0
```

## Implementation Notes

### Allocation Tracking

```lisp
;; Using sb-sprof in allocation mode
(require :sb-sprof)

(defun track-allocation (code)
  (let ((start-bytes (sb-ext:get-bytes-consed)))
    (sb-sprof:with-profiling
        (:mode :alloc
         :max-samples 1000
         :report :flat)
      (eval (read-from-string code)))
    (let ((end-bytes (sb-ext:get-bytes-consed)))
      (- end-bytes start-bytes))))
```

### Memory Report

```lisp
(defun generate-memory-report (&key gc-first)
  (when gc-first
    (gc :full t))

  (let ((dynamic-usage (sb-vm::dynamic-usage))
        (static-usage (sb-vm::static-space-usage))
        (read-only-usage (sb-vm::read-only-space-usage)))

    (list :dynamic-space dynamic-usage
          :static-space static-usage
          :read-only-space read-only-usage
          :gc-stats (sb-ext:gc-stats))))
```

### GC Statistics

```lisp
(defun extract-gc-stats ()
  (let ((stats (sb-ext:gc-stats)))
    (list
      :total-collections (length stats)
      :total-gc-time (reduce #'+ stats :key #'gc-stat-time)
      :gen-0-collections (count 0 stats :key #'gc-stat-gen)
      :gen-1-collections (count 1 stats :key #'gc-stat-gen)
      :gen-2-collections (count 2 stats :key #'gc-stat-gen))))
```

### Allocation Attribution

```lisp
;; Parse sb-sprof allocation profile
(defun extract-allocation-hot-spots (profile-output)
  (let ((lines (split-lines profile-output)))
    (loop for line in lines
          when (allocation-data-line-p line)
          collect
            (make-allocation-entry
              :function (extract-function-name line)
              :samples (extract-sample-count line)
              :bytes (extract-byte-count line)
              :percentage (extract-percentage line)))))
```

## Related Properties

- **sampling-accuracy**: Accurate samples → correct allocation hot spots
- **profiling-overhead-bounded**: Low overhead → valid measurements
- **hot-spot-identification**: Allocation hot spots correctly ranked
- **timing-accuracy**: GC time accurately measured
