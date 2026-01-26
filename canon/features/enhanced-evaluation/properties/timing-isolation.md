---
type: property
name: timing-isolation
version: 0.1.0
feature: enhanced-evaluation
covers:
  - contracts/time-execution-tool
---

# Timing Isolation Property

## Statement

**For all** code executed via `time-execution`,
**timing measurements** MUST exclude MCP protocol overhead and measure only user code execution.

## Formal Expression

```
∀ code ∈ ExecutableCode :
  let timing = time-execution(code).timing
  let overhead = mcp_overhead(code)
  then:
    timing.real_time_ms = pure_execution_time(code) + ε_measurement
    timing.real_time_ms ≠ (pure_execution_time(code) + overhead)

where:
  mcp_overhead(code) = {
    json_parse_time,
    json_serialize_time,
    message_framing_time,
    tool_dispatch_time,
    result_formatting_time
  }

  pure_execution_time(code) = {
    code_reading_and_parsing,
    compilation_if_needed,
    actual_execution,
    output_capture,
    gc_triggered_by_code
  }

  ε_measurement ≤ 0.1ms  (intrinsic measurement overhead)
```

## Informal Explanation

Timing must measure user code execution only:

**Included in timing**:
1. Reading and parsing the code string
2. Compilation (if code not previously compiled)
3. Actual code execution
4. Output capture from `*standard-output*`
5. GC triggered by user code allocation
6. I/O performed by user code

**Excluded from timing**:
1. JSON parsing of MCP request
2. JSON serialization of MCP response
3. MCP message framing (JSONRPC envelope)
4. Tool name dispatch and routing
5. Parameter validation
6. Result formatting for JSON

The timing boundaries are:
- **Start**: Just before reading code string
- **End**: Just after capturing result value

This ensures timing reflects only the cost of the user's code.

## Rationale

Timing isolation is critical for:
- **Accurate profiling**: Only user code performance measured
- **Reproducibility**: Same code always times similarly
- **Comparability**: Different tools have different overhead
- **Trust**: Users trust measurements reflect their code

If MCP overhead was included:
- Fast code would show inflated times
- Profiling would be misleading
- Comparison would be invalid
- Tool would be unreliable

## Counterexample Shape

**MCP Overhead Included**:
```lisp
;; Instant execution
(time-execution "(+ 1 2 3)")

;; Returns:
;; {
;;   "timing": {
;;     "real-time-ms": 50.0  ; VIOLATION!
;;   }
;; }
;; Should be ~0.01ms - most of this is JSON parsing overhead
```

**Serialization Time Included**:
```lisp
;; Code that produces large result
(time-execution "(make-list 100000)")

;; Returns:
;; {
;;   "timing": {
;;     "real-time-ms": 150.0  ; VIOLATION if includes serialization
;;   }
;; }
;; Serializing 100K element list to JSON is expensive
;; Should not be included in timing
```

**Tool Dispatch Included**:
```lisp
;; Empty code
(time-execution "nil")

;; Returns:
;; {
;;   "timing": {
;;     "real-time-ms": 25.0  ; VIOLATION!
;;   }
;; }
;; Should be negligible - evaluating NIL is instant
```

## Verification Approach

**Property Test Strategy**:

1. **Minimal Code Timing**:
   ```lisp
   (let ((result (time-execution "nil")))
     ;; Evaluating nil should be near-instant
     ;; If timing is >1ms, likely includes overhead
     (assert (< (result-real-time-ms result) 1.0)))
   ```

2. **Consistent Fast Code**:
   ```lisp
   ;; Run same fast code multiple times
   (let ((timings (loop repeat 10
                       collect (result-real-time-ms
                                (time-execution "(+ 1 2 3)")))))
     ;; Should all be similar (small variance)
     ;; High variance suggests overhead contamination
     (assert (< (standard-deviation timings) 0.5)))
   ```

3. **Sleep Accuracy**:
   ```lisp
   (let ((result (time-execution "(sleep 0.1)")))
     ;; Should be ~100ms regardless of MCP overhead
     ;; If significantly more, overhead is included
     (assert (< (result-real-time-ms result) 105.0))
     (assert (> (result-real-time-ms result) 100.0)))
   ```

4. **CPU-Bound Stability**:
   ```lisp
   ;; Known CPU-bound work
   (let ((r1 (time-execution "(loop repeat 100000 sum 1)"))
         (r2 (time-execution "(loop repeat 100000 sum 1)")))
     ;; Two runs should be similar
     ;; MCP overhead would add constant offset
     (assert (< (abs (- (result-real-time-ms r1)
                       (result-real-time-ms r2)))
               0.5)))
   ```

5. **Large Result No Impact**:
   ```lisp
   ;; Small vs large result, same computation
   (let ((r1 (time-execution "(loop repeat 1000 sum 1)"))
         (r2 (time-execution "(loop repeat 1000 collect 1)")))
     ;; Both do 1000 iterations
     ;; r2 has larger result to serialize, but timing should be similar
     ;; (only difference is cons allocation during execution)
     (assert (< (/ (result-real-time-ms r1)
                  (result-real-time-ms r2))
               2.0)))
   ```

6. **Measurement Overhead Only**:
   ```lisp
   ;; The only overhead should be time capture itself
   (let ((result (time-execution "(progn)")))
     ;; Empty progn - pure measurement overhead
     (assert (< (result-real-time-ms result) 0.1)))
   ```

**Isolation Validator**:

```lisp
(defun verify-timing-isolation ()
  ;; Measure overhead outside tool
  (let ((start (get-internal-real-time)))
    (let ((result (time-execution "(+ 1 2 3)")))
      (let* ((end (get-internal-real-time))
             (total-time (/ (* (- end start) 1000.0)
                           internal-time-units-per-second))
             (reported-time (result-real-time-ms result)))

        ;; Total time includes MCP overhead
        ;; Reported time should be much smaller
        (assert (< reported-time (* total-time 0.5)))))))
```

**Timing Boundary Verification**:

```lisp
(defun verify-timing-boundaries ()
  (let ((code "(progn (print 'START) (sleep 0.05) (print 'END))"))
    ;; Capture output to verify execution happened
    (let ((result (time-execution code)))
      (and
        ;; Timing should be ~50ms (just the sleep + negligible print)
        (> (result-real-time-ms result) 50.0)
        (< (result-real-time-ms result) 55.0)

        ;; Output should be captured (proves execution happened)
        (search "START" (result-output result))
        (search "END" (result-output result))))))
```

**Edge Cases**:

- Empty code - should show only measurement overhead
- Very fast code (<0.1ms) - overhead percentage higher but absolute value small
- Very slow code (>10s) - overhead percentage negligible
- Code with errors - timing still isolated to execution attempt
- Code with heavy output - output capture included, serialization excluded
- Nested tool calls - each isolated independently

**Implementation Requirements**:

```lisp
(defun handle-time-execution-tool (request)
  ;; MCP OVERHEAD - NOT TIMED
  (let ((params (parse-json-request request)))
    (let ((code (get-param params "code"))
          (package (get-param params "package" "CL-USER")))

      ;; TIMING STARTS HERE
      (let ((timing-start (get-internal-real-time))
            (bytes-start (sb-ext:get-bytes-consed)))

        (in-package package)

        ;; User code execution (TIMED)
        (multiple-value-bind (result output error)
            (capture-execution code)

          ;; TIMING ENDS HERE
          (let ((timing-end (get-internal-real-time))
                (bytes-end (sb-ext:get-bytes-consed)))

            ;; Calculate timing (still not part of timing)
            (let ((timing-data (compute-timing
                                 timing-start timing-end
                                 bytes-start bytes-end)))

              ;; MCP OVERHEAD - NOT TIMED
              (format-json-response
                result output error timing-data))))))))
```

**Timing Capture Points**:

```
┌─────────────────────────────────────────────────────────────┐
│ MCP Request Received                                         │
│   ↓                                                           │
│ Parse JSON          ← OVERHEAD (not timed)                   │
│   ↓                                                           │
│ Dispatch to tool    ← OVERHEAD (not timed)                   │
│   ↓                                                           │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ START TIMING ← get-internal-real-time                   │ │
│ │   ↓                                                      │ │
│ │ Read code string                                        │ │
│ │   ↓                                                      │ │
│ │ Parse Lisp forms                                        │ │
│ │   ↓                                                      │ │
│ │ Evaluate code                                           │ │
│ │   ↓                                                      │ │
│ │ Capture output                                          │ │
│ │   ↓                                                      │ │
│ │ END TIMING ← get-internal-real-time                     │ │
│ └─────────────────────────────────────────────────────────┘ │
│   ↓                                                           │
│ Format result       ← OVERHEAD (not timed)                   │
│   ↓                                                           │
│ Serialize JSON      ← OVERHEAD (not timed)                   │
│   ↓                                                           │
│ Send response       ← OVERHEAD (not timed)                   │
└─────────────────────────────────────────────────────────────┘
```

**Measurement Intrinsic Overhead**:

The timing mechanism itself has minimal overhead:
- 2x `get-internal-real-time` calls: ~0.001ms
- 2x `get-internal-run-time` calls: ~0.001ms
- 2x `sb-ext:get-bytes-consed` calls: ~0.005ms
- Arithmetic and delta calculation: ~0.001ms
- **Total**: ~0.01ms

This overhead is **included** in timing (unavoidable) but is negligible for most code.

**Shrinking**: Find minimal code where timing incorrectly includes MCP overhead
