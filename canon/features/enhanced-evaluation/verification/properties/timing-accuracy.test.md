---
type: verification
name: timing-accuracy-property-test
source: properties/timing-accuracy.md
level: property
tags:
  - property-based
  - timing
  - accuracy
---

# Property Test: Timing Accuracy

## Purpose

Verify that `time-execution` accurately measures real-time, run-time, GC time, and memory allocation across various workloads.

## Prerequisites

- Initialized MCP server
- Property-based testing framework

## Implementation

### Generator: Timed Workloads

```lisp
(defun generate-sleep-code (duration-ms)
  "Generate code that sleeps for specified duration"
  (format nil "(sleep ~,3f)" (/ duration-ms 1000.0)))

(defun generate-computation-code (iterations)
  "Generate CPU-bound computation"
  (format nil "(loop repeat ~D sum i)" iterations))

(defun generate-allocation-code (count)
  "Generate code that allocates known amount"
  (format nil "(make-list ~D)" count))

(defun generate-timed-workload ()
  "Generate workload with known timing characteristics"
  (random-choice
   (list
    ;; Sleep workloads (real-time dominated)
    (generate-sleep-code (random-in-range 10 200))

    ;; CPU workloads (run-time dominated)
    (generate-computation-code (random-in-range 10000 1000000))

    ;; Allocation workloads
    (generate-allocation-code (random-in-range 1000 50000))

    ;; Mixed
    (format nil "(progn ~A ~A)"
            (generate-sleep-code 50)
            (generate-computation-code 10000)))))
```

### Property: Sleep Reflects Real Time

```lisp
(deftest timing-accuracy-sleep-real-time ()
  "Sleep duration reflected in real-time measurement"
  (dotimes (i 100)
    (let* ((sleep-ms (+ 20 (random 100)))  ; 20-120ms
           (code (generate-sleep-code sleep-ms))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing)))

      ;; Real time should be >= sleep time (within ±10ms tolerance)
      (is (>= real-time (- sleep-ms 10))
          "Sleep ~Ams: real-time ~Ams too low" sleep-ms real-time)
      (is (<= real-time (+ sleep-ms 20))
          "Sleep ~Ams: real-time ~Ams too high" sleep-ms real-time))))
```

### Property: Allocation Tracking Accuracy

```lisp
(deftest timing-accuracy-allocation-tracking ()
  "Memory allocation accurately tracked"
  (dotimes (i 100)
    (let* ((list-size (+ 1000 (random 10000)))
           (code (generate-allocation-code list-size))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (bytes-allocated (gethash "bytes-allocated" timing))
           ;; Each cons cell is 16 bytes on 64-bit SBCL
           (expected-bytes (* list-size 16)))

      ;; Should allocate approximately expected amount (±20% tolerance)
      (is (>= bytes-allocated (* expected-bytes 0.8))
          "List size ~A: allocated ~A bytes, expected ~A"
          list-size bytes-allocated expected-bytes)
      (is (<= bytes-allocated (* expected-bytes 1.5))
          "List size ~A: allocated ~A bytes too much" list-size bytes-allocated))))
```

### Property: Zero Allocation Detection

```lisp
(deftest timing-accuracy-zero-allocation ()
  "Simple arithmetic shows zero or minimal allocation"
  (dotimes (i 100)
    (let* ((code (random-choice
                  '("(+ 1 2 3)"
                    "(* 4 5 6)"
                    "(- 100 50)"
                    "(min 10 20 30)"
                    "(max 1 2 3)")))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (bytes-allocated (gethash "bytes-allocated" timing)))

      ;; Should allocate nothing or very little (<1KB overhead)
      (is (<= bytes-allocated 1024)
          "Code ~A allocated ~A bytes, expected ~0" code bytes-allocated))))
```

### Property: Run Time vs Real Time

```lisp
(deftest timing-accuracy-run-vs-real ()
  "Run-time <= Real-time for all executions"
  (dotimes (i 200)
    (let* ((code (generate-timed-workload))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing))
           (run-time (gethash "run-time-ms" timing)))

      ;; Run-time should never exceed real-time
      (is (<= run-time (* real-time 1.1))  ; Allow 10% measurement variance
          "Code ~A: run-time (~A) > real-time (~A)" code run-time real-time))))
```

### Property: GC Time Bounded

```lisp
(deftest timing-accuracy-gc-time-bounded ()
  "GC time <= Real-time"
  (dotimes (i 100)
    (let* ((code (generate-allocation-code (+ 10000 (random 40000))))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing))
           (gc-time (gethash "gc-time-ms" timing)))

      ;; GC time should not exceed real time
      (is (<= gc-time real-time)
          "Code ~A: gc-time (~A) > real-time (~A)" code gc-time real-time))))
```

### Property: Allocation Rate Calculation

```lisp
(deftest timing-accuracy-allocation-rate ()
  "Allocation rate correctly calculated from bytes/time"
  (dotimes (i 100)
    (let* ((code (generate-allocation-code 5000))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (bytes-allocated (gethash "bytes-allocated" timing))
           (real-time-ms (gethash "real-time-ms" timing))
           (reported-rate (gethash "allocation-rate-mb-per-sec" timing))
           ;; Calculate expected rate
           (real-time-sec (/ real-time-ms 1000.0))
           (bytes-per-sec (/ bytes-allocated real-time-sec))
           (expected-rate (/ bytes-per-sec 1048576.0)))  ; Convert to MB/s

      ;; Rates should match (within 5% tolerance)
      (when (> bytes-allocated 1000)  ; Skip if negligible allocation
        (is (<= (abs (- reported-rate expected-rate))
                (* expected-rate 0.05))
            "Allocation rate mismatch: reported ~A, expected ~A"
            reported-rate expected-rate)))))
```

### Property: Timing Stability

```lisp
(deftest timing-accuracy-stability ()
  "Repeated execution of same code gives similar timing"
  (dotimes (i 20)
    (let ((code "(loop repeat 100000 sum i)")
          (timings '()))

      ;; Execute 5 times
      (dotimes (j 5)
        (let* ((response (call-tool *test-server* "time-execution"
                                   `(("code" . ,code))))
               (timing (extract-timing response))
               (run-time (gethash "run-time-ms" timing)))
          (push run-time timings)))

      ;; Calculate coefficient of variation
      (let* ((mean (/ (reduce #'+ timings) (length timings)))
             (variance (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2))
                                             timings))
                         (length timings)))
             (std-dev (sqrt variance))
             (cv (/ std-dev mean)))

        ;; Coefficient of variation should be < 0.3 (30%)
        (is (< cv 0.3)
            "Timing instable: CV=~A for timings ~A" cv timings)))))
```

### Property: Error Case Timing

```lisp
(deftest timing-accuracy-with-errors ()
  "Timing captured even when code errors"
  (dotimes (i 100)
    (let* ((error-code (random-choice
                        '("(/ 1 0)"
                          "(car 'not-a-list)"
                          "(elt '(1 2 3) 999)"
                          "(error \"boom\")")))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,error-code))))
           (timing (extract-timing response)))

      ;; Timing should still be present
      (is (not (null timing))
          "No timing for error code: ~A" error-code)
      (is (> (gethash "real-time-ms" timing 0) 0)
          "Zero real-time for error code: ~A" error-code))))
```

## Configuration

- Examples: 100-200 per test
- Timeout: 5 minutes total
- Measurement tolerance: 10-20% for timing, 20% for allocation

## Assertions

For every generated workload:
1. Real-time >= run-time (approximately)
2. GC-time <= real-time
3. Sleep duration reflected in real-time
4. Allocation tracked accurately (±20%)
5. Zero allocation for simple arithmetic
6. Allocation rate = bytes / time
7. Repeated runs have similar timing (CV < 30%)

## Helper Functions

```lisp
(defun extract-timing (response)
  "Extract timing hash-table from response"
  (let ((content (result-content response)))
    (parse-timing-json content)))

(defun parse-timing-json (json-text)
  "Parse timing section from JSON response"
  ;; Simplified parser for testing
  (let ((timing (make-hash-table :test 'equal)))
    (setf (gethash "real-time-ms" timing)
          (extract-number json-text "real-time-ms"))
    (setf (gethash "run-time-ms" timing)
          (extract-number json-text "run-time-ms"))
    (setf (gethash "gc-time-ms" timing)
          (extract-number json-text "gc-time-ms"))
    (setf (gethash "bytes-allocated" timing)
          (extract-number json-text "bytes-allocated"))
    (setf (gethash "allocation-rate-mb-per-sec" timing)
          (extract-number json-text "allocation-rate"))
    timing))

(defun random-in-range (min max)
  "Random integer in [min, max)"
  (+ min (random (- max min))))
```

## Notes

- Timing measurements have inherent variance from system load
- Tolerances account for measurement overhead and GC pauses
- Sleep tests most reliable for real-time accuracy
- Allocation tracking depends on SBCL internals
- Run multiple times if tests flaky due to system load
