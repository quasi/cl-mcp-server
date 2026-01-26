---
type: contract
name: time-execution-tool
version: 0.1.0
---

# Time-Execution Tool Contract

Execute code with detailed timing and memory allocation information. Returns
real time, run time, GC time, and bytes allocated.

## Tool Definition

```json
{
  "name": "time-execution",
  "description": "Execute code with detailed timing and memory allocation information. Returns real time, run time, GC time, and bytes allocated. Useful for profiling and performance analysis.",
  "inputSchema": {
    "type": "object",
    "required": ["code"],
    "properties": {
      "code": {
        "type": "string",
        "description": "Common Lisp code to execute and time"
      },
      "package": {
        "type": "string",
        "description": "Package context for execution (default: CL-USER)"
      }
    }
  }
}
```

## Input Processing

### Execution with Timing

1. Switch to specified package (default `CL-USER`)
2. Read forms from `code` string
3. Capture `sb-ext:get-bytes-consed` before execution
4. Record start time (both real-time and run-time)
5. Execute code within `handler-case` for errors
6. Capture end time and bytes-consed
7. Calculate timing deltas and format results
8. Return result value, output, and timing statistics

### Package Handling

| Scenario | Behavior |
|----------|----------|
| Package not specified | Use `CL-USER` |
| Package not found | Return error |
| Multiple forms | Execute all, return last value |

## Output Format

### Success Response

```json
{
  "result": "42",
  "output": "",
  "timing": {
    "real-time-ms": 15.234,
    "run-time-ms": 14.891,
    "gc-time-ms": 0.003,
    "bytes-allocated": 16384,
    "allocation-rate-mb-per-sec": 1.024
  }
}
```

### Timing Fields

| Field | Unit | Description |
|-------|------|-------------|
| `real-time-ms` | Milliseconds | Wall-clock elapsed time |
| `run-time-ms` | Milliseconds | CPU time in user code |
| `gc-time-ms` | Milliseconds | Time spent in GC |
| `bytes-allocated` | Bytes | Heap allocation during execution |
| `allocation-rate-mb-per-sec` | MB/s | Allocation rate |

### Timing Precision

- Times reported with microsecond precision (0.001 ms)
- Real-time uses `get-internal-real-time`
- Run-time uses `get-internal-run-time`
- GC time from SBCL `gc-run-time` statistics

## Timing Accuracy

### MCP Overhead Exclusion

Timing should **exclude**:
- JSON parsing and serialization
- Message framing overhead
- Tool dispatch time
- Result formatting time

Timing should **include only**:
- User code execution
- Any I/O performed by user code
- Compilation if not pre-compiled
- GC triggered by user code

### Measurement Strategy

```lisp
;; Start timing
(let ((bytes-before (sb-ext:get-bytes-consed))
      (real-start (get-internal-real-time))
      (run-start (get-internal-run-time)))

  ;; Execute user code
  (let ((result (progn ...user code...)))

    ;; End timing
    (let ((real-end (get-internal-real-time))
          (run-end (get-internal-run-time))
          (bytes-after (sb-ext:get-bytes-consed)))

      ;; Calculate deltas
      (format-timing-results ...))))
```

## Examples

### Quick Computation

Input:
```json
{
  "code": "(+ 1 2 3)",
  "package": "CL-USER"
}
```

Output:
```json
{
  "result": "6",
  "output": "",
  "timing": {
    "real-time-ms": 0.023,
    "run-time-ms": 0.021,
    "gc-time-ms": 0.000,
    "bytes-allocated": 0,
    "allocation-rate-mb-per-sec": 0.0
  }
}
```

### Allocation-Heavy Code

Input:
```json
{
  "code": "(loop repeat 10000 collect (make-list 100))"
}
```

Output:
```json
{
  "result": "#<RESULT TOO LARGE>",
  "output": "",
  "timing": {
    "real-time-ms": 12.456,
    "run-time-ms": 11.234,
    "gc-time-ms": 1.123,
    "bytes-allocated": 16384000,
    "allocation-rate-mb-per-sec": 1315.2
  }
}
```

### Code with Output

Input:
```json
{
  "code": "(dotimes (i 3) (print i))"
}
```

Output:
```json
{
  "result": "NIL",
  "output": "0\n1\n2\n",
  "timing": {
    "real-time-ms": 0.234,
    "run-time-ms": 0.198,
    "gc-time-ms": 0.000,
    "bytes-allocated": 512,
    "allocation-rate-mb-per-sec": 2.342
  }
}
```

### Error During Execution

Input:
```json
{
  "code": "(/ 1 0)"
}
```

Output:
```json
{
  "isError": true,
  "errorType": "DIVISION-BY-ZERO",
  "errorMessage": "arithmetic error DIVISION-BY-ZERO signalled",
  "timing": {
    "real-time-ms": 0.045,
    "run-time-ms": 0.043,
    "gc-time-ms": 0.000,
    "bytes-allocated": 256,
    "allocation-rate-mb-per-sec": 5.463
  }
}
```

Note: Timing is still captured even when errors occur.

## Performance Characteristics

### Overhead

The timing infrastructure itself has minimal overhead:
- ~0.001ms for time capture
- ~0.005ms for bytes-consed queries
- ~0.010ms for result formatting

For very fast code (<0.1ms), the overhead percentage may be significant.
The tool makes no attempt to subtract this overhead.

### GC Impact

Garbage collection can significantly affect timing:
- GC may be triggered by user code
- GC time is separately tracked
- Large allocations may cause multiple GC cycles
- `(gc :full t)` before timing gives more consistent results

## Implementation Notes

### Internal Time Units

SBCL uses internal time units (ITU):
- `internal-time-units-per-second` = 1000 (typically)
- Convert to milliseconds: `(* (- end start) 1000.0 / ITUPS)`

### Bytes Consed Monotonic Counter

`sb-ext:get-bytes-consed` is a monotonic counter that increases
throughout the session. Take deltas between measurements.

### Result Truncation

Very large results are truncated to avoid overwhelming JSON responses:
- Use `*print-length*` = 100
- Use `*print-level*` = 5
- Show `#<RESULT TOO LARGE>` if exceeds 10KB

## Verification Strategy

Tests should verify:

1. **Timing accuracy**: Compare with expected execution time
2. **Allocation tracking**: Verify bytes-allocated matches expectations
3. **GC accounting**: GC time is non-zero for GC-triggering code
4. **Error timing**: Errors still produce timing data
5. **Zero-allocation path**: Simple arithmetic shows zero allocation
6. **Overhead isolation**: MCP overhead not included in timing
