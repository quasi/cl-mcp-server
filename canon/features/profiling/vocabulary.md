# Profiling Vocabulary

Terms specific to performance profiling and memory analysis.

---

## Profiling

**Profiling** is the measurement of program performance to identify:

- Hot spots (frequently executed code)
- Time-consuming functions
- Memory allocation patterns
- Resource bottlenecks

Two main approaches:
- **Statistical**: Sample call stack periodically
- **Deterministic**: Instrument every function call

---

## Statistical Profiling

**Statistical Profiling** samples the program counter at regular intervals:

1. Start profiler
2. Run code
3. Interrupt periodically (e.g., every 10ms)
4. Record current call stack
5. Aggregate samples into report

Pros:
- Low overhead (~5-10%)
- Works on any code
- Finds hot spots naturally

Cons:
- Non-deterministic (different each run)
- May miss infrequent code

---

## Sample

A **Sample** is one observation of the call stack:

```
Sample #142:
  0: FOO
  1: BAR
  2: BAZ
  3: MAIN
```

Samples are aggregated to estimate time spent in each function.

---

## Flat Profile

A **Flat Profile** shows time per function (excluding callees):

```
Function        Samples    Self%    Cumulative%
----------------------------------------------
COMPUTE-HEAVY      450     45.0%         45.0%
PROCESS-DATA       300     30.0%         75.0%
VALIDATE           150     15.0%         90.0%
MAIN               100     10.0%        100.0%
```

"Self%" = time in function itself (exclusive).

---

## Call Graph Profile

A **Call Graph Profile** shows time including callees (inclusive):

```
MAIN (100% inclusive, 10% exclusive)
  └─ PROCESS-DATA (75% inclusive, 30% exclusive)
      ├─ COMPUTE-HEAVY (45% inclusive, 45% exclusive)
      └─ VALIDATE (15% inclusive, 15% exclusive)
```

Inclusive time = function + everything it calls.

---

## Deterministic Profiling

**Deterministic Profiling** instruments functions to track exact counts:

```lisp
(sb-profile:profile foo bar baz)
(run-test)
(sb-profile:report)
```

Records:
- Exact call count
- Exact time spent (user + system)
- Consing (allocation) per function

Higher overhead (~30-100%) but precise.

---

## sb-sprof

**sb-sprof** is SBCL's statistical profiler:

```lisp
(require :sb-sprof)
(sb-sprof:with-profiling (:mode :cpu :report :flat)
  (my-code))
```

Modes:
- `:cpu` - CPU time (default)
- `:time` - Wall-clock time
- `:alloc` - Allocation profiling

---

## sb-profile

**sb-profile** is SBCL's deterministic profiler:

```lisp
(require :sb-profile)
(sb-profile:profile foo bar)
(my-code)
(sb-profile:report)
(sb-profile:unprofile-all)
```

Tracks exact metrics but high overhead.

---

## Hot Spot

A **Hot Spot** is code that consumes significant execution time:

- Tight loops
- Frequently called functions
- Expensive operations

Profiling identifies hot spots for optimization.

---

## Call Count

**Call Count** is the number of times a function is invoked:

```
Function         Calls    Time/Call
------------------------------------
FAST-PATH     1000000      0.001ms
SLOW-PATH          10     50.000ms
```

High call count functions are optimization targets.

---

## Allocation Profiling

**Allocation Profiling** tracks memory allocation:

```
Function            Bytes      %
-------------------------------
MAKE-OBJECTS    16384000    80%
BUILD-LIST       2048000    10%
PARSE-INPUT      1024000     5%
```

Identifies allocation hot spots.

---

## Memory Report

A **Memory Report** shows heap usage:

- Dynamic space used/total
- Static space used
- Read-only space used
- GC statistics (collections, time)

From `sb-ext:gc` and `room` functionality.

---

## GC Statistics

**GC Statistics** track garbage collection:

| Metric | Description |
|--------|-------------|
| Collections | Number of GC runs |
| GC time | Total time in GC |
| Gen 0/1/2 | Generational GC stats |
| Efficiency | (Freed / Total) ratio |

High GC time indicates allocation pressure.

---

## Consing

**Consing** is allocation of heap memory:

```lisp
(time (loop repeat 1000 collect (cons 1 2)))
;; => consed 32000 bytes
```

Reducing consing improves performance by reducing GC pressure.

---

## Sampling Interval

The **Sampling Interval** is time between profile samples:

- Shorter interval = more samples = higher accuracy = more overhead
- Longer interval = fewer samples = lower accuracy = less overhead

Typical: 10ms (100 samples/second)

---

## Profile Report

A **Profile Report** presents profiling results:

Types:
- **Flat**: Functions sorted by self-time
- **Graph**: Call tree with inclusive times
- **Annotated**: Source code with per-line costs

Generated after profiling completes.

---

## Instrumentation Overhead

**Instrumentation Overhead** is the cost added by profiling:

- Statistical: 5-10% overhead
- Deterministic: 30-100% overhead
- Allocation tracking: 10-20% overhead

Must account for overhead in measurements.

---

## Generation (GC)

A **Generation** in generational GC:

- **Gen 0**: Nursery (young objects)
- **Gen 1**: Intermediate
- **Gen 2**: Tenured (old objects)

SBCL uses generational GC to optimize collection.

---

## Dynamic Space

**Dynamic Space** is the heap where most objects are allocated:

```lisp
(room)
;; Dynamic space usage is: 123456789 bytes
```

Grows as needed up to heap limit.
