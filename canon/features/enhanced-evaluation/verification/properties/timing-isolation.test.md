---
type: verification
name: timing-isolation-property-test
source: properties/timing-isolation.md
level: property
tags:
  - property-based
  - timing
  - isolation
---

# Property Test: Timing Isolation

## Purpose

Verify that `time-execution` measurements exclude MCP protocol overhead and measure only user code execution.

## Prerequisites

- Initialized MCP server
- Ability to measure total request time

## Implementation

### Property: Minimal Code Low Overhead

```lisp
(deftest timing-isolation-minimal-code ()
  "Minimal code shows negligible timing"
  (dotimes (i 100)
    (let* ((code (random-choice '("nil" "(+ 1 2)" "(list)" "t")))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing)))

      ;; Minimal code should be < 1ms
      ;; If much higher, likely includes MCP overhead
      (is (< real-time 2.0)
          "Minimal code ~A took ~Ams (expected < 2ms)" code real-time))))
```

### Property: Sleep Timing Precision

```lisp
(deftest timing-isolation-sleep-precision ()
  "Sleep timing unaffected by overhead"
  (dotimes (i 50)
    (let* ((sleep-ms (+ 50 (random 100)))
           (code (format nil "(sleep ~,3f)" (/ sleep-ms 1000.0)))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing)))

      ;; Should match sleep duration within ±5ms
      (is (>= real-time (- sleep-ms 5))
          "Sleep ~Ams: timing ~Ams too low" sleep-ms real-time)
      (is (<= real-time (+ sleep-ms 10))
          "Sleep ~Ams: timing ~Ams too high (overhead?)" sleep-ms real-time))))
```

### Property: Timing Consistency

```lisp
(deftest timing-isolation-consistency ()
  "Same code gives consistent timing across runs"
  (dotimes (outer 20)
    (let ((code "(loop repeat 50000 sum i)")
          (timings '()))

      ;; Run 5 times
      (dotimes (i 5)
        (let* ((response (call-tool *test-server* "time-execution"
                                   `(("code" . ,code))))
               (timing (extract-timing response))
               (run-time (gethash "run-time-ms" timing)))
          (push run-time timings)))

      ;; Calculate coefficient of variation
      (let* ((mean (/ (reduce #'+ timings) (length timings)))
             (std-dev (sqrt (/ (reduce #'+ (mapcar
                                            (lambda (x) (expt (- x mean) 2))
                                            timings))
                              (length timings))))
             (cv (/ std-dev mean)))

        ;; CV should be < 0.2 (20%) for consistency
        ;; High CV suggests overhead varies between runs
        (is (< cv 0.25)
            "High timing variance: CV=~A, timings=~A" cv timings)))))
```

### Property: Result Size No Impact

```lisp
(deftest timing-isolation-result-size ()
  "Large result doesn't inflate timing"
  (dotimes (i 30)
    (let* ((iterations 5000)
           ;; Same work, different result sizes
           (small-result (format nil "(loop repeat ~D sum 1)" iterations))
           (large-result (format nil "(loop repeat ~D collect i)" iterations))
           (r1 (call-tool *test-server* "time-execution"
                         `(("code" . ,small-result))))
           (r2 (call-tool *test-server* "time-execution"
                         `(("code" . ,large-result))))
           (t1 (gethash "run-time-ms" (extract-timing r1)))
           (t2 (gethash "run-time-ms" (extract-timing r2))))

      ;; Timings should be similar (within 3x)
      ;; Large difference suggests serialization time included
      (is (< (/ (max t1 t2) (max (min t1 t2) 0.01)) 3.0)
          "Result size affected timing: small=~Ams, large=~Ams" t1 t2))))
```

### Property: Error Timing Isolated

```lisp
(deftest timing-isolation-error-timing ()
  "Error handling doesn't inflate timing"
  (dotimes (i 50)
    (let* ((error-code (random-choice
                        '("(/ 1 0)"
                          "(car 'not-a-list)"
                          "(error \"test\")")))
           (response (call-tool *test-server* "time-execution"
                               `(("code" . ,error-code))))
           (timing (extract-timing response))
           (real-time (gethash "real-time-ms" timing)))

      ;; Error path should be fast
      (is (< real-time 5.0)
          "Error code ~A took ~Ams (overhead?)" error-code real-time))))
```

### Property: Comparative Timing

```lisp
(deftest timing-isolation-comparative ()
  "Relative timing between fast/slow code correct"
  (let* ((fast-code "(+ 1 2 3)")
         (slow-code "(loop repeat 100000 sum i)")
         (r-fast (call-tool *test-server* "time-execution"
                           `(("code" . ,fast-code))))
         (r-slow (call-tool *test-server* "time-execution"
                           `(("code" . ,slow-code))))
         (t-fast (gethash "run-time-ms" (extract-timing r-fast)))
         (t-slow (gethash "run-time-ms" (extract-timing r-slow))))

    ;; Slow code should be significantly slower
    ;; If overhead dominates, ratio would be small
    (is (> t-slow (* t-fast 10))
        "Slow code not proportionally slower: fast=~Ams, slow=~Ams"
        t-fast t-slow)))
```

### Property: Output Capture Included

```lisp
(deftest timing-isolation-output-included ()
  "Output capture time included in timing (part of execution)"
  (let* ((code "(dotimes (i 100) (print i))")
         (response (call-tool *test-server* "time-execution"
                             `(("code" . ,code))))
         (timing (extract-timing response))
         (real-time (gethash "real-time-ms" timing)))

    ;; Should be measurable (printing takes time)
    (is (> real-time 0.5)
        "Print loop timing seems too low: ~Ams" real-time)))
```

## Helper Functions

```lisp
(defun extract-timing (response)
  "Extract timing hash-table from tool response"
  (let ((content (result-content response)))
    (parse-timing-json content)))

(defun random-choice (list)
  "Pick random element"
  (nth (random (length list)) list))
```

## Configuration

- Examples: 30-100 per test
- Focus on boundary cases (very fast, very slow, errors)
- Timing variance acceptable but should be bounded

## Notes

- MCP overhead (JSON parsing, serialization) must not appear in timing
- Only user code execution and intrinsic measurement overhead included
- Timing boundaries: start before code reading, end after result capture
- Serialization of result to JSON happens AFTER timing ends
