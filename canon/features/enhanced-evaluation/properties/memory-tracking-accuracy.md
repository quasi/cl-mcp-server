---
type: property
name: memory-tracking-accuracy
version: 0.1.0
feature: enhanced-evaluation
covers:
  - contracts/time-execution-tool
---

# Memory Tracking Accuracy Property

## Statement

**For all** code executed via `time-execution`,
**memory allocation measurements** (bytes-allocated, allocation-rate) MUST accurately reflect actual heap allocation within measurement precision.

## Formal Expression

```
∀ code ∈ ExecutableCode, ∀ ε ∈ MeasurementError :
  let metrics = time-execution(code).timing
  let actual_alloc = measure-actual-allocation(code)
  then:
    |metrics.bytes_allocated - actual_alloc.bytes| ≤ ε_alloc
    metrics.allocation_rate_mb_per_sec =
      (metrics.bytes_allocated / (metrics.real_time_ms / 1000.0)) / (1024 * 1024)

where:
  ε_alloc ≤ 1024 bytes  (allocation tracking precision)
  actual_alloc = Δ(sb-ext:get-bytes-consed)
```

## Informal Explanation

Memory allocation tracking must be accurate:

1. **Bytes allocated**: Exact delta of heap allocation
   - Measured via `sb-ext:get-bytes-consed`
   - Captures all cons cells, vectors, objects, etc.
   - Includes objects later garbage collected

2. **Allocation rate**: Correctly derived from bytes and time
   - Formula: `bytes / (real_time_sec) / (1024 * 1024)` for MB/s
   - Zero when no allocation occurs
   - Proportional to allocation amount

3. **Precision**: Byte-accurate allocation tracking
   - No estimation or sampling
   - Monotonic counter delta
   - Captures all heap activity

This ensures Claude can accurately analyze memory usage patterns.

## Rationale

Accurate memory tracking is essential for:
- Identifying allocation-heavy code paths
- Comparing memory efficiency of algorithms
- Detecting memory leaks or excessive allocation
- Understanding GC pressure

Inaccurate tracking would:
- Hide memory inefficiencies
- Lead to wrong optimization decisions
- Make profiling unreliable
- Prevent memory analysis

## Counterexample Shape

**Incorrect Byte Count**:
```lisp
(time-execution "(make-list 10000)")

;; Returns:
;; {
;;   "bytes-allocated": 0  ; VIOLATION!
;; }
;; Should show ~160KB (cons cells: 10000 * 16 bytes)
```

**Wrong Allocation Rate**:
```lisp
(time-execution "(loop repeat 1000 collect (cons 1 2))")

;; Returns:
;; {
;;   "bytes-allocated": 16000,
;;   "real-time-ms": 10.0,
;;   "allocation-rate-mb-per-sec": 1000.0  ; VIOLATION!
;; }
;; Correct rate: 16000 / (10/1000) / (1024*1024) ≈ 1.526 MB/s
```

**Missing Allocation**:
```lisp
(time-execution "(make-array 100000 :element-type 'double-float)")

;; Returns:
;; {
;;   "bytes-allocated": 100  ; VIOLATION!
;; }
;; Should show ~800KB (100000 * 8 bytes for doubles)
```

**False Allocation**:
```lisp
(time-execution "(+ 1 2 3)")

;; Returns:
;; {
;;   "bytes-allocated": 5000  ; VIOLATION!
;; }
;; Simple fixnum arithmetic allocates nothing (should be 0)
```

## Verification Approach

**Property Test Strategy**:

1. **Zero Allocation Path**:
   ```lisp
   (let ((result (time-execution "(+ 1 2 3)")))
     ;; Fixnum arithmetic should not allocate
     (assert (= (result-bytes-allocated result) 0))
     (assert (= (result-allocation-rate result) 0.0)))
   ```

2. **Known Cons Cell Allocation**:
   ```lisp
   (let ((result (time-execution "(make-list 1000)")))
     ;; Each cons = 16 bytes on 64-bit SBCL
     ;; Should allocate ~16000 bytes
     (assert (>= (result-bytes-allocated result) 15000))
     (assert (<= (result-bytes-allocated result) 17000)))
   ```

3. **Array Allocation**:
   ```lisp
   (let ((result (time-execution
                   "(make-array 10000 :element-type 'double-float)")))
     ;; Double-float array: header + 10000*8 bytes
     (assert (>= (result-bytes-allocated result) 80000)))
   ```

4. **String Allocation**:
   ```lisp
   (let ((result (time-execution
                   "(make-string 50000 :initial-element #\\a)")))
     ;; String: header + character data
     (assert (>= (result-bytes-allocated result) 50000)))
   ```

5. **Allocation Rate Calculation**:
   ```lisp
   (let* ((result (time-execution "(make-list 10000)"))
          (bytes (result-bytes-allocated result))
          (time-sec (/ (result-real-time-ms result) 1000.0))
          (expected-rate (/ bytes time-sec (* 1024 1024))))
     ;; Allow 1% tolerance for floating point
     (assert (< (abs (- (result-allocation-rate result)
                       expected-rate))
               (* expected-rate 0.01))))
   ```

6. **Large Allocation**:
   ```lisp
   (let ((result (time-execution
                   "(loop repeat 100000 collect (make-list 10))")))
     ;; 100000 top-level cons + 100000*10 inner cons
     ;; ~17.6 MB total
     (assert (> (result-bytes-allocated result) 16000000)))
   ```

7. **Repeated Allocation**:
   ```lisp
   (let ((result (time-execution
                   "(dotimes (i 1000) (cons i i))")))
     ;; 1000 cons cells = ~16000 bytes
     (assert (>= (result-bytes-allocated result) 15000)))
   ```

8. **Incremental Allocation**:
   ```lisp
   ;; Run same code multiple times
   (let ((r1 (time-execution "(make-list 100)"))
         (r2 (time-execution "(make-list 200)")))
     ;; Allocation should scale linearly
     (assert (< (abs (- (result-bytes-allocated r2)
                       (* 2 (result-bytes-allocated r1))))
               1000)))
   ```

**Allocation Measurement Validator**:

```lisp
(defun verify-allocation-accuracy (code expected-bytes tolerance)
  (let ((result (time-execution code)))
    (let ((actual (result-bytes-allocated result))
          (min-expected (- expected-bytes tolerance))
          (max-expected (+ expected-bytes tolerance)))
      (and
        ;; Bytes in expected range
        (>= actual min-expected)
        (<= actual max-expected)

        ;; Rate correctly calculated
        (let* ((time-sec (/ (result-real-time-ms result) 1000.0))
               (expected-rate (if (> time-sec 0)
                                 (/ actual time-sec (* 1024 1024))
                                 0.0)))
          (< (abs (- (result-allocation-rate result)
                    expected-rate))
             0.01))))))
```

**Direct Measurement Comparison**:

```lisp
(defun compare-with-direct-measurement (code)
  ;; Measure directly
  (let ((bytes-before (sb-ext:get-bytes-consed)))
    (eval (read-from-string code))
    (let* ((bytes-after (sb-ext:get-bytes-consed))
           (direct-delta (- bytes-after bytes-before)))

      ;; Measure via tool
      (let ((tool-delta (result-bytes-allocated
                         (time-execution code))))

        ;; Should match within small tolerance
        ;; (multiple runs may vary slightly due to environment)
        (< (abs (- tool-delta direct-delta))
          1024)))))
```

**Edge Cases**:

- Zero allocation code - bytes = 0, rate = 0.0
- Very fast allocation - rate may be very high
- GC during execution - bytes = total allocated, not current live set
- Errors during execution - allocation before error still tracked
- Very large allocation - ensure no integer overflow
- Allocation in tight loop - ensure all tracked

**Implementation Requirements**:

```lisp
(defun execute-with-allocation-tracking (code)
  ;; Capture bytes-consed before execution
  (let ((bytes-before (sb-ext:get-bytes-consed))
        (real-start (get-internal-real-time)))

    ;; Execute code
    (let ((result (eval-code code)))

      ;; Capture after state
      (let* ((bytes-after (sb-ext:get-bytes-consed))
             (real-end (get-internal-real-time))

             ;; Calculate allocation
             (bytes-delta (- bytes-after bytes-before))

             ;; Calculate time
             (real-ms (/ (* (- real-end real-start) 1000.0)
                        internal-time-units-per-second))

             ;; Calculate rate (MB/s)
             (alloc-rate (if (> real-ms 0)
                           (/ bytes-delta
                              (/ real-ms 1000.0)
                              (* 1024 1024))
                           0.0)))

        (make-result
          :bytes-allocated bytes-delta
          :allocation-rate-mb-per-sec alloc-rate
          :real-time-ms real-ms
          :result result)))))
```

**Allocation Patterns to Test**:

1. **Cons cells**: `(cons a b)`, `(list ...)`, `(append ...)`
2. **Vectors**: `(make-array n)`, `(vector ...)`
3. **Strings**: `(make-string n)`, `(format nil ...)`
4. **Objects**: `(make-instance ...)`, struct allocation
5. **Closures**: Lambda captures
6. **Bignums**: Large integer arithmetic
7. **Floats**: `(coerce n 'double-float)`

**Precision Guarantees**:

- Bytes allocated: exact (no estimation)
- Allocation rate: 3 decimal places (microsecond time precision)
- No rounding errors for small allocations

**Shrinking**: Find minimal code where allocation measurement is incorrect
