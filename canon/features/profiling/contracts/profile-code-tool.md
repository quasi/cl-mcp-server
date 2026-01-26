---
type: contract
name: profile-code-tool
version: 0.1.0
---

# Profile-Code Tool Contract

Profile code using statistical sampling. Runs code while collecting stack
samples to identify hot spots. Supports CPU time, wall-clock time, or
allocation profiling modes.

## Tool Definition

```json
{
  "name": "profile-code",
  "description": "Profile code using statistical sampling. Runs the code while collecting stack samples to identify hot spots. Supports CPU time, wall-clock time, or allocation profiling modes.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp code to profile"
      },
      "package": {
        "type": "string",
        "description": "Package context for evaluation (default: CL-USER)"
      },
      "mode": {
        "type": "string",
        "enum": ["cpu", "time", "alloc"],
        "description": "Profiling mode: cpu (default), time (wall-clock), or alloc (memory)"
      },
      "max-samples": {
        "type": "integer",
        "description": "Maximum samples to collect (default: 1000)"
      },
      "sample-interval": {
        "type": "number",
        "description": "Seconds between samples (default: 0.01)"
      },
      "report-type": {
        "type": "string",
        "enum": ["flat", "graph"],
        "description": "Report format: flat (default) or graph"
      }
    }
  }
}
```

## Input Processing

### Profiling Execution

1. Switch to specified package (default `CL-USER`)
2. Configure sb-sprof with mode and parameters
3. Start profiling
4. Execute code
5. Stop profiling
6. Generate report in requested format
7. Return results and timing

### Profiling Modes

| Mode | Measures | Use Case |
|------|----------|----------|
| `cpu` | CPU time | Find CPU-bound hot spots |
| `time` | Wall-clock | Find blocking/I/O hot spots |
| `alloc` | Allocation | Find memory allocation hot spots |

### Package Handling

| Scenario | Behavior |
|----------|----------|
| Package not specified | Use `CL-USER` |
| Package not found | Return error |
| Multiple forms | Execute all, profile entire run |

## Output Format

### Flat Profile Report

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

Top 3 hot spots account for 88% of execution time.

Result: <computation complete>
```

### Graph Profile Report

```
Statistical Profile (CPU mode)
Total samples: 842

Call Graph (inclusive times):

MAIN [100.0%]
  ├─ PROCESS-LOOP [95.2%]
  │   ├─ COMPUTE-HEAVY [54.2%]
  │   ├─ PROCESS-DATA [22.2%]
  │   │   └─ VALIDATE-INPUT [11.6%]
  │   └─ FORMAT-OUTPUT [5.3%]
  └─ INITIALIZE [4.8%]

Note: Percentages show inclusive time (function + callees)

Result: <computation complete>
```

### Allocation Profile

```
Statistical Profile (ALLOC mode)
Total samples: 523
Sample interval: 0.01s

Function                    Samples    Bytes      %
---------------------------------------------------
BUILD-LIST                     298    9.5 MB    57.0%
MAKE-OBJECTS                   145    4.6 MB    27.7%
PROCESS-STRING                  52    1.7 MB     9.9%
COPY-STRUCTURE                  18    0.6 MB     3.4%
(Other functions)               10    0.3 MB     2.0%

Total allocation: 16.7 MB

Result: <list of 10000 objects>
```

## Statistical Sampling

### How It Works

1. **Set timer interrupt**: Wake up every N milliseconds
2. **Capture stack**: Record current call stack
3. **Aggregate**: Count occurrences of each function
4. **Report**: Functions with most samples are hot spots

### Sample Interpretation

- **Sample count**: How often function was executing
- **Self %**: Time in function itself (excluding callees)
- **Cumulative %**: Running total (sorted by self %)

If a function has 54% of samples, it consumed ~54% of execution time.

### Accuracy Factors

- **Duration**: Longer runs → more samples → higher accuracy
- **Sample interval**: Shorter → more overhead but more accurate
- **Hot spot significance**: Functions must run long enough to be sampled

### Minimum Requirements

For reliable profiling:
- Code should run at least 0.5 seconds
- At least 100 samples collected
- Hot spots should consume >5% of time

## Profiling Modes Detail

### CPU Mode (`:cpu`)

Samples when process is actively using CPU.

**Best for**:
- Computational algorithms
- Number crunching
- CPU-bound operations

**Won't show**:
- I/O waits
- Sleep time
- Blocking operations

### Time Mode (`:time`)

Samples based on wall-clock time.

**Best for**:
- I/O-bound code
- Network operations
- Overall program behavior

**Shows**:
- Everything (CPU + waits)
- Time spent in sleep
- Blocking I/O

### Allocation Mode (`:alloc`)

Samples on memory allocation events.

**Best for**:
- Finding allocation hot spots
- Reducing GC pressure
- Memory optimization

**Shows**:
- Which functions allocate most
- Consing hot spots

## Examples

### CPU Profiling

Input:
```json
{
  "code": "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)",
  "mode": "cpu",
  "report-type": "flat"
}
```

Output:
```
Statistical Profile (CPU mode)
Total samples: 1456

Function            Samples    Self%    Cumulative%
-------------------------------------------------
FIB                    1423    97.7%         97.7%
+                        18     1.2%         98.9%
<=                       10     0.7%         99.6%
(Other)                   5     0.4%        100.0%

Result: 832040
Duration: 14.56s
```

### Allocation Profiling

Input:
```json
{
  "code": "(loop repeat 10000 collect (make-list 100))",
  "mode": "alloc",
  "report-type": "flat"
}
```

Output:
```
Statistical Profile (ALLOC mode)
Total samples: 345

Function            Samples    Bytes      %
------------------------------------------
MAKE-LIST              298    9.5 MB    86.4%
LOOP                    34    1.1 MB     9.9%
CONS                    13    0.4 MB     3.7%

Total allocation: 11.0 MB

Result: #<LIST of 10000 elements>
Duration: 0.23s
```

### Graph Report

Input:
```json
{
  "code": "(defun a () (loop repeat 1000000 sum (b))) (defun b () (random 100)) (a)",
  "mode": "cpu",
  "report-type": "graph"
}
```

Output:
```
Statistical Profile (CPU mode)

Call Graph:

A [100.0%]
  └─ B [78.5%]
      └─ RANDOM [76.2%]

Function details:
  A: 100.0% inclusive, 21.5% exclusive (210 samples)
  B: 78.5% inclusive, 2.3% exclusive (23 samples)
  RANDOM: 76.2% inclusive, 76.2% exclusive (762 samples)

Result: 49685034
```

### Fast Code (Insufficient Samples)

Input:
```json
{
  "code": "(+ 1 2 3)",
  "mode": "cpu"
}
```

Output:
```
Statistical Profile (CPU mode)
Total samples: 0

Warning: Code executed too quickly to collect samples.
(Duration: <0.01s)

For reliable profiling, code should run at least 0.5 seconds.

Result: 6
```

## Implementation Notes

### Using sb-sprof

```lisp
(require :sb-sprof)

(sb-sprof:with-profiling
    (:mode :cpu
     :max-samples 1000
     :sample-interval 0.01
     :report :flat)
  (user-code))
```

### Report Generation

```lisp
;; Generate report as string
(with-output-to-string (s)
  (sb-sprof:report :type :flat :stream s))
```

### Sample Collection

- Samples stored in memory during profiling
- Memory usage: ~100 bytes per sample
- 1000 samples = ~100 KB

### Overhead

- CPU mode: ~5-10% overhead
- Time mode: ~5-10% overhead
- Alloc mode: ~10-20% overhead

## Error Response

When profiling fails:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{error message}\n\n[Backtrace]\n{formatted backtrace}"
    }
  ],
  "isError": true
}
```

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| `reader-error` | Malformed code string | "Error reading code" |
| `package-error` | Invalid package name | "Package X not found" |
| Any runtime error | Error during profiled execution | Error with backtrace |
| `sb-ext:timeout` | Code exceeds timeout | Timeout message |
| `storage-condition` | Out of memory | Memory error |

### Error During Profiled Code

If the profiled code signals an error, profiling stops and the error is reported:

Input:
```json
{
  "code": "(error \"intentional error\")",
  "mode": "cpu"
}
```

Response:
```
[ERROR] SIMPLE-ERROR
intentional error

[Backtrace]
0: (ERROR "intentional error")
1: (EVAL (ERROR "intentional error"))
...

(Profiling stopped due to error)
```

### Invalid Mode

Input:
```json
{
  "code": "(+ 1 2)",
  "mode": "invalid"
}
```

Response:
```
[ERROR] SIMPLE-ERROR
Invalid profiling mode: "invalid". Valid modes: cpu, time, alloc
```

## Verification Strategy

Tests should verify:

1. **Hot spot identification**: Known hot spot appears in report
2. **Sample collection**: Adequate samples collected
3. **Mode differences**: CPU vs time modes differ for I/O code
4. **Allocation tracking**: Alloc mode shows allocation hot spots
5. **Fast code**: Appropriate warning for too-fast code
6. **Report formats**: Both flat and graph reports work
7. **Result preserved**: Code result returned correctly
8. **Error handling**: Errors during profiling are caught
