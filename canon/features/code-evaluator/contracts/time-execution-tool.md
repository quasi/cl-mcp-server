---
type: contract
name: time-execution-tool
version: 0.1.0
phase: B
---

# Time-Execution Tool Contract

Executes code with detailed timing and memory allocation information. Designed for profiling and performance analysis.

## Tool Definition

```json
{
  "name": "time-execution",
  "description": "Execute code with detailed timing and memory allocation information. Returns real time, run time, GC time, and bytes allocated.",
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

## Output Format

### Success Response

```
Timing:
  Real time:      42 ms
  Run time:       38 ms
  GC time:        2 ms
  Bytes consed:   1,048,576

Result:
  <return value>

Output:
<any stdout output>
```

### Error Response

```
Error during execution:
[ERROR] DIVISION-BY-ZERO
...
```

## Timing Metrics

| Metric | Description |
|--------|-------------|
| Real time | Wall-clock time elapsed (includes I/O, scheduling) |
| Run time | CPU time used by the Lisp process |
| GC time | Time spent in garbage collection |
| Bytes consed | Total memory allocated (may include GC'd memory) |

## Implementation

Uses SBCL-specific facilities:

```lisp
(let ((start-real (get-internal-real-time))
      (start-run (get-internal-run-time))
      (start-gc sb-ext:*gc-run-time*)
      (start-bytes (sb-ext:get-bytes-consed)))
  ;; Execute code
  ;; Calculate deltas and format output
  )
```

## Use Cases

1. **Performance Profiling**: Measure execution time of algorithms
2. **Memory Analysis**: Track memory allocation patterns
3. **GC Investigation**: Identify GC-heavy operations
4. **Optimization Validation**: Compare before/after performance

## Comparison with evaluate-lisp

| Aspect | time-execution | evaluate-lisp |
|--------|----------------|---------------|
| Primary purpose | Profiling | General evaluation |
| Timing output | Always shown first | Optional, shown after result |
| Output format | Structured profiling | General purpose |
| Best for | Performance analysis | Code execution |

## Notes

- Timing resolution is in milliseconds
- Bytes consed includes all allocations, including short-lived objects
- GC time is cumulative across all GC runs during execution
- For accurate measurements, run code multiple times and average
