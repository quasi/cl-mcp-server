# Why Persistent Sessions?

**Understanding the design rationale behind CL-MCP-Server's persistent session model.**

Unlike traditional code execution services that evaluate code in isolation, CL-MCP-Server maintains a persistent REPL (Read-Eval-Print Loop) session. This document explains why this design choice matters and what benefits it provides.

## The Problem with Stateless Execution

Many code execution systems use a stateless model:

```
Request 1: Evaluate (defun square (x) (* x x))
Response: Function defined ✓

Request 2: Evaluate (square 5)
Response: ERROR - undefined function SQUARE
```

**Why it fails**: Each request starts from a clean slate. The function defined in Request 1 doesn't exist in Request 2.

**Workarounds**:
- Include all definitions in every request (verbose, inefficient)
- Store state externally (complex, slow)
- Pre-load code from files (inflexible)

These workarounds add complexity and friction to the development experience.

## The REPL Model

CL-MCP-Server uses a persistent session model, like a traditional REPL:

```
Request 1: Evaluate (defun square (x) (* x x))
Response: => SQUARE

Request 2: Evaluate (square 5)
Response: => 25

Request 3: Evaluate (defparameter *scale* 10)
Response: => *SCALE*

Request 4: Evaluate (* (square 5) *scale*)
Response: => 250
```

**Why it works**: State accumulates across evaluations. Functions, variables, and loaded libraries persist throughout the session.

## Benefits of Persistent Sessions

### 1. Natural Interactive Development

Persistent sessions match how developers actually work:

```
Step 1: Define a helper function
(defun normalize (x) (/ x 100.0))

Step 2: Define a function that uses it
(defun process-score (score)
  (* (normalize score) 5.0))

Step 3: Test it
(process-score 80)  ;; => 4.0

Step 4: Refine it
(defun process-score (score)
  (let ((normalized (normalize score)))
    (if (< normalized 0.5)
        (* normalized 3.0)  ; Boost low scores
        (* normalized 5.0))))

Step 5: Test again
(process-score 80)  ;; => 4.0
(process-score 30)  ;; => 0.9 (boosted)
```

Without persistence, you'd need to resend all definitions at each step.

### 2. Incremental Exploration

Build understanding progressively:

```
;; Explore a data structure
(defvar *data* '((name . "Alice") (age . 30) (city . "NYC")))

;; Extract parts
(cdr (assoc 'name *data*))  ;; => "Alice"

;; Build a helper
(defun get-field (field data)
  (cdr (assoc field data)))

;; Use it
(get-field 'age *data*)  ;; => 30

;; Refine it
(defun get-field (field data &optional default)
  (let ((pair (assoc field data)))
    (if pair (cdr pair) default)))

;; Test refinement
(get-field 'email *data* "unknown")  ;; => "unknown"
```

Each step builds on the previous one naturally.

### 3. Library Loading Efficiency

Load libraries once, use them throughout:

```
;; Load at start of session
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

;; Use freely in subsequent evaluations
(alexandria:flatten '((1 2) (3 4)))
(cl-ppcre:split "\\s+" "hello world")

;; No need to reload
```

In a stateless model, you'd need to reload libraries for every evaluation, adding significant latency.

### 4. Debugging and Refinement

Fix bugs without redefining everything:

```
;; Initial (buggy) version
(defun calculate-discount (price)
  (* price 0.1))  ; 10% discount

;; Test
(calculate-discount 100)  ;; => 10.0

;; Refine based on requirements
(defun calculate-discount (price)
  (cond
    ((>= price 1000) (* price 0.2))  ; 20% for high values
    ((>= price 100) (* price 0.1))   ; 10% for medium
    (t (* price 0.05))))              ; 5% for low

;; Test again immediately
(calculate-discount 100)   ;; => 10.0
(calculate-discount 1000)  ;; => 200.0
```

Only the changed function needs to be resent.

### 5. Exploratory Data Analysis

Work with data interactively:

```
;; Load data
(defvar *sales-data*
  '((jan . 1000) (feb . 1200) (mar . 1100)))

;; Explore
(reduce #'+ (mapcar #'cdr *sales-data*))  ;; Total: 3300

;; Create analysis functions
(defun average (data)
  (/ (reduce #'+ (mapcar #'cdr data))
     (length data)))

(defun trend (data)
  (let ((values (mapcar #'cdr data)))
    (- (car (last values)) (car values))))

;; Analyze
(average *sales-data*)  ;; => 1100
(trend *sales-data*)    ;; => 100 (growing)
```

## What Persists?

Understanding what stays and what doesn't:

### Persists Across Evaluations

- **Functions**: `defun`, `defmethod`, `defgeneric`
- **Macros**: `defmacro`
- **Variables**: `defvar`, `defparameter`, `defconstant`, `setf`
- **Classes**: `defclass`, `defstruct`
- **Packages**: `defpackage`, `in-package`
- **Loaded Systems**: `ql:quickload`, `asdf:load-system`
- **Package State**: Current package (`*package*`)

### Does NOT Persist

- **Let Bindings**: Local variables from `let`/`let*`
- **Dynamic Bindings**: Temporarily bound special variables
- **Catch Tags**: `catch`/`throw` blocks
- **Restarts**: `restart-case` handlers
- **Handler Bindings**: `handler-bind` handlers

### Example: What Persists

```lisp
;; Evaluation 1
(defun outer ()
  (let ((x 10))  ; Local binding
    (defun inner ()
      (* x 2)))
  (inner))

(outer)  ;; => 20

;; Evaluation 2
(inner)  ;; => 20 (inner persists, captures x=10)

;; Evaluation 3
(outer)  ;; => 20 (x is rebound to 10 each time)
```

## Session Lifecycle

### Session Creation

A new session is created when:
- The MCP server starts
- Claude Code connects to the server

Initial state:
- Package: `CL-USER`
- No user-defined symbols
- Standard Common Lisp library loaded

### During Session

State accumulates:
- Each `defun` adds a function
- Each `defvar` adds a variable
- Each `ql:quickload` loads a system

State can be modified:
- Functions can be redefined
- Variables can be reset
- Packages can be switched

### Session End

The session ends when:
- The MCP server process terminates
- Claude Code disconnects

All session state is lost when the session ends.

## Trade-offs

### Advantages

- **Natural workflow**: Matches REPL development
- **Efficiency**: No need to resend definitions
- **Exploration-friendly**: Build understanding incrementally
- **Performance**: Libraries loaded once

### Disadvantages

- **State accumulation**: Session can become cluttered
- **No automatic cleanup**: Old definitions remain unless explicitly removed
- **Hidden dependencies**: Functions may rely on implicit state
- **Reproducibility**: Sequence of evaluations matters

### Mitigation Strategies

**1. Explicit State Management**

```lisp
;; Clear a function
(fmakunbound 'old-function)

;; Unbind a variable
(makunbound '*old-var*)
```

**2. Package Isolation**

```lisp
;; Work in separate packages
(defpackage :experiment
  (:use :cl))

(in-package :experiment)

;; Clean experiments don't pollute CL-USER
```

**3. Fresh Start When Needed**

```lisp
;; Future: reset-session tool
;; For now: restart Claude Code
```

**4. Document Dependencies**

```lisp
(defun process-order (order)
  "Requires: *discount-rate* to be defined"
  (* (order-total order) (- 1 *discount-rate*)))
```

## Comparison with Other Models

### Traditional REPL

**Similarity**: State persists across evaluations

**Difference**: CL-MCP-Server uses JSON-RPC over stdio instead of terminal I/O

### Jupyter Notebooks

**Similarity**: Cells share state, execution order matters

**Difference**: CL-MCP-Server doesn't have cells—it's pure sequential evaluation

### AWS Lambda / Cloud Functions

**Similarity**: Both execute user code

**Difference**: Lambda is stateless by design; CL-MCP-Server maintains state

### Docker Containers

**Similarity**: Both provide isolated environments

**Difference**: Containers persist until explicitly stopped; CL-MCP-Server session ends when server stops

## Implementation Details

### How State is Maintained

- **Single Lisp Image**: One SBCL process per server instance
- **No Serialization**: State lives in memory, never saved to disk
- **No Forking**: Each evaluation happens in the same process
- **Package Isolation**: Different sessions (different server processes) don't share state

### Safety Mechanisms

- **Error Isolation**: Errors in user code don't crash the server
- **Output Capture**: Stdout/stderr are captured without affecting server logging
- **Condition Handling**: All conditions are caught and reported
- **Infinite Loop Protection**: (Future: timeout mechanism)

## Use Cases

### Ideal For

- **Interactive Development**: Building functions step by step
- **Exploratory Programming**: Understanding unfamiliar APIs
- **Data Analysis**: Working with datasets interactively
- **Prototyping**: Rapid experimentation
- **Learning**: Teaching Common Lisp concepts

### Not Ideal For

- **Reproducible Builds**: Use ASDF system definitions instead
- **Production Deployment**: Use compiled Lisp applications
- **Parallel Execution**: One session = one sequential context
- **Stateless Services**: Use HTTP-based stateless APIs

## Future Enhancements

Planned improvements to session management:

1. **reset-session tool**: Clear all state, start fresh
2. **list-definitions tool**: See what's defined in current session
3. **save-session / load-session**: Persist session to disk
4. **session snapshots**: Checkpoint and restore state
5. **timeout protection**: Prevent infinite loops

## Conclusion

Persistent sessions make CL-MCP-Server feel like a natural extension of Common Lisp's interactive development philosophy. The REPL model, where state accumulates and you build programs incrementally, is fundamental to the Lisp experience.

By maintaining session state, CL-MCP-Server enables:
- Natural, incremental development workflows
- Efficient library loading
- Exploratory programming and data analysis
- A smooth developer experience

The trade-off is that you must be aware of accumulated state, but this is the same trade-off faced by all REPL-based development—and Lisp developers have been managing this successfully for decades.

## See Also

- [Session State Contract](../../canon/features/session-management/contracts/session-state.md) - What persists and what doesn't (formal specification)
- [How to Switch Packages](../how-to/switch-packages.md) - Managing package context
- [Architecture Explanation](architecture.md) - How the server maintains state internally
- [State Persistence Scenario](../../canon/features/session-management/scenarios/state-persistence.md) - Concrete examples
