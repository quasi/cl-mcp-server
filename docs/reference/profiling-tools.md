# Profiling and Performance Tools Reference

<!-- Generated from: canon/features/profiling/contracts/*.md -->

Tools for analyzing performance, identifying bottlenecks, and optimizing Common Lisp code.

## Overview

Profiling tools help you understand where your code spends time and allocates memory:
- Identify CPU-intensive functions (hot spots)
- Find memory allocation bottlenecks
- Measure wall-clock time (including I/O waits)
- Generate actionable performance reports

## profile-code

Profile code execution using statistical sampling to identify performance bottlenecks.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| code | string | Yes | Common Lisp code to profile |
| package | string | No | Package context (default: CL-USER) |
| mode | string | No | Profiling mode: `cpu`, `time`, or `alloc` (default: `cpu`) |
| max-samples | integer | No | Maximum samples to collect (default: 1000) |
| sample-interval | number | No | Seconds between samples (default: 0.01) |
| report-type | string | No | Report format: `flat` or `graph` (default: `flat`) |

### Profiling Modes

| Mode | Measures | Best For |
|------|----------|----------|
| **cpu** | CPU time spent in functions | CPU-bound algorithms, computation |
| **time** | Wall-clock time | I/O-bound code, blocking operations |
| **alloc** | Memory allocation | Finding allocation hot spots, GC pressure |

### Output

**Flat Profile Report:**

```
Statistical Profile (CPU mode)
Total samples: 842
Sample interval: 0.01s
Duration: 8.42s

Function                    Samples    Self%    Cumulative%
----------------------------------------------------------
COMPUTE-HEAVY                  456    54.2%         54.2%
PROCESS-DATA                   187    22.2%         76.4%
VALIDATE-INPUT                  98    11.6%         88.0%
FORMAT-OUTPUT                   45     5.3%         93.3%
HELPER-FUNCTION                 32     3.8%         97.1%
(Other functions)               24     2.9%        100.0%

Code result: 42
```

**Graph Report:**

```
Statistical Profile (CPU mode)
Call graph (parent → child relationships):

MAIN-FUNCTION (100%)
  └─ COMPUTE-HEAVY (54.2%)
       ├─ PROCESS-DATA (22.2%)
       │   └─ VALIDATE-INPUT (11.6%)
       └─ FORMAT-OUTPUT (5.3%)
            └─ HELPER-FUNCTION (3.8%)
```

### Examples

**Profile CPU-intensive code:**

```
User: Please profile-code: "(dotimes (i 1000000) (sqrt i))"

Response:
Statistical Profile (CPU mode)
Total samples: 312
Sample interval: 0.01s
Duration: 3.12s

Function                    Samples    Self%    Cumulative%
----------------------------------------------------------
SQRT                           289    92.6%         92.6%
DOTIMES                         23     7.4%        100.0%

Code result: NIL
```

**Profile memory allocation:**

```
User: Please profile-code with mode="alloc":
     "(loop for i from 1 to 10000 collect (make-list i))"

Response:
Statistical Profile (ALLOC mode)
Total samples: 487
Bytes allocated: 834,567,890

Function                    Samples    Bytes        %
------------------------------------------------------
MAKE-LIST                      423    721,234,567  86.4%
LOOP                            64    113,333,323  13.6%

Code result: <list of lists>
```

**Profile I/O-bound code:**

```
User: Please profile-code with mode="time":
     "(with-open-file (s \"large-file.txt\") (read-line s))"

Response:
Statistical Profile (TIME mode - wall-clock)
Total samples: 156
Duration: 1.56s

Function                    Samples    Self%    Cumulative%
----------------------------------------------------------
READ-LINE                      134    85.9%         85.9%
WITH-OPEN-FILE                  22    14.1%        100.0%

Code result: "First line of file"
```

**Full call graph:**

```
User: Please profile-code with report-type="graph":
     "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
      (factorial 10000)"

Response:
Statistical Profile (CPU mode)
Call graph:

FACTORIAL (100%)
  └─ FACTORIAL (recursive, 67.3%)
       └─ * (multiplication, 28.4%)
       └─ - (subtraction, 4.3%)

Code result: <very large number>
```

### Reading the Results

**Samples Column:**
- Number of times the profiler caught execution in this function
- Higher samples = more time spent

**Self%:**
- Percentage of total time in THIS function (not including callees)
- Focus optimization here

**Cumulative%:**
- Running total percentage
- Top functions account for most time

### Optimization Strategy

1. **Identify hot spots** - Functions with highest Self%
2. **Verify call frequency** - Is it called often or just slow?
3. **Optimize the bottleneck** - Focus on the top 1-3 functions first
4. **Re-profile** - Measure improvement
5. **Repeat** - Until performance is acceptable

### Common Use Cases

**Finding slow algorithms:**
```json
{
  "code": "(my-slow-function input-data)",
  "mode": "cpu",
  "report-type": "graph"
}
```

**Debugging allocation issues:**
```json
{
  "code": "(process-large-dataset data)",
  "mode": "alloc",
  "max-samples": 5000
}
```

**Analyzing I/O performance:**
```json
{
  "code": "(fetch-and-process-files)",
  "mode": "time",
  "sample-interval": 0.1
}
```

### Sample Interval Guidelines

| Interval | Use Case |
|----------|----------|
| 0.001s (1ms) | Very fast code, fine-grained profiling |
| 0.01s (10ms) | Default, good for most cases |
| 0.1s (100ms) | Long-running code, reduce overhead |
| 1.0s | Very long operations (minutes) |

### Notes

- Uses SBCL's `sb-sprof` (statistical profiler)
- Profiling adds overhead (usually 1-5%)
- Code executes normally and returns its result
- More samples = more accurate profile, but longer runtime
- Graph mode shows call relationships
- Flat mode shows absolute time per function

### Limitations

- Statistical profiling (not deterministic)
- Can't profile code that runs too quickly (< 0.1s)
- Inlined functions may not appear
- Built-in functions shown as system internals
- Requires code to actually run (can't profile hypothetically)

### Tips

**Profile representative workloads:**
- Use realistic input data
- Run enough iterations for statistical significance
- Profile production-like scenarios

**Compare before/after:**
```
1. Profile original code
2. Make optimization
3. Profile optimized code
4. Verify improvement
```

**Focus on low-hanging fruit:**
- Optimize functions with >10% Self%
- Avoid micro-optimizations (<1% functions)
- Profile again after each change

---

## See Also

- [time-execution](enhanced-evaluation.md#time-execution) - Simple execution timing (no profiling)
- [evaluate-lisp](evaluate-lisp.md) - Execute code without profiling
- [How to: Optimize Performance](../how-to/optimize-performance.md) - Practical optimization guide
