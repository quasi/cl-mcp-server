---
type: property
name: backtrace-accuracy
version: 0.1.0
feature: error-intelligence
covers:
  - contracts/describe-last-error-tool
relates_to:
  - error-capture-completeness
---

# Backtrace Accuracy Property

## Statement

**For all** errors that occur during evaluation,
**the backtrace** returned by error intelligence tools MUST accurately reflect the actual call chain from the error point to the top level.

## Formal Expression

```
∀ code ∈ ErrorProducingCode, ∀ error ∈ Conditions :
  let eval_result = evaluate-lisp(code)
  when eval_result.isError = true
  then:
    let backtrace = get-backtrace()
    let actual_stack = capture-actual-call-stack(error)

    1. backtrace.frames matches actual_stack.frames
    2. ∀ i ∈ [0..min(|backtrace|, |actual_stack|)] :
       backtrace[i].function = actual_stack[i].function
    3. Frame ordering preserved (most recent first)
    4. No spurious frames inserted
    5. No actual frames omitted (up to max-frames limit)

where:
  Frame = {
    number: Integer,
    function: String,
    arguments: Optional[List],
    source: Optional[Location]
  }

  actual_stack captured at error signal point
  backtrace returned by get-backtrace tool
```

## Informal Explanation

When an error occurs, the call stack at that moment represents the actual sequence of function calls leading to the error. The backtrace returned by `get-backtrace` must faithfully represent this stack:

1. **Function Names**: Each frame must show the correct function name
2. **Call Order**: Frames must be in correct order (innermost/most recent first)
3. **Completeness**: All frames present up to the specified limit
4. **No Fabrication**: No frames added that weren't in the actual stack
5. **Frame Numbers**: Sequential numbering from 0

The backtrace is the primary tool for understanding the execution path that led to an error. Inaccurate backtraces mislead debugging efforts.

## Rationale

Accurate backtraces are essential for:
- **Debugging**: Understanding the execution path
- **Root Cause Analysis**: Finding where the error originated
- **Code Understanding**: Seeing how functions call each other
- **Trust**: Developers must trust the debugging information

Inaccurate backtraces cause:
- **Misdirected debugging**: Looking at wrong functions
- **Lost time**: Following false leads
- **Loss of trust**: Can't rely on the tool
- **Incomplete understanding**: Missing critical call context

The backtrace must represent the SBCL debugger's view of the stack at the error point, without:
- Omitting internal frames (unless explicitly limited)
- Reordering frames
- Misidentifying function names
- Adding non-existent frames

## Counterexample Shape

If this property is violated:

**Missing Frames**:
```lisp
;; Code with nested calls
(evaluate-lisp
  "(defun inner () (/ 1 0))
   (defun middle () (inner))
   (defun outer () (middle))
   (outer)")

;; Actual stack: OUTER -> MIDDLE -> INNER -> /
;; But get-backtrace shows:
[
  {"frame": 0, "function": "/"}
  {"frame": 1, "function": "OUTER"}
  ; VIOLATION! Missing MIDDLE and INNER frames
]
```

**Wrong Function Names**:
```lisp
(evaluate-lisp "(defun my-func () (error \"test\")) (my-func)")

;; get-backtrace shows:
[
  {"frame": 0, "function": "UNKNOWN"}  ; VIOLATION! Should be ERROR
  {"frame": 1, "function": "LAMBDA"}   ; VIOLATION! Should be MY-FUNC
]
```

**Incorrect Ordering**:
```lisp
(evaluate-lisp "(defun a () (b)) (defun b () (/ 1 0)) (a)")

;; Actual stack: A -> B -> /
;; But get-backtrace shows reversed:
[
  {"frame": 0, "function": "/"}
  {"frame": 1, "function": "A"}  ; VIOLATION! Should be B
  {"frame": 2, "function": "B"}  ; VIOLATION! Should be A
]
```

**Fabricated Frames**:
```lisp
(evaluate-lisp "(/ 1 0)")

;; Simple error, should show just / and EVAL
;; But get-backtrace adds extra frames:
[
  {"frame": 0, "function": "/"}
  {"frame": 1, "function": "PHANTOM-WRAPPER"}  ; VIOLATION! Not real
  {"frame": 2, "function": "EVAL"}
]
```

**Truncation Without Indication**:
```lisp
;; Deep call stack with 50 frames
;; get-backtrace(max-frames=20) returns:
;; Only 20 frames but doesn't indicate more exist
;; VIOLATION! Should indicate truncation
```

## Verification Approach

**Generator**: Generate code with known call structures

**Assertion**:
```lisp
(defun verify-backtrace-accuracy (code expected-call-chain)
  ;; Execute code that will error
  (let ((result (evaluate-lisp code)))
    (assert (result-is-error result))

    ;; Get backtrace
    (let ((backtrace (get-backtrace)))

      (and
        ;; Has frames
        (> (length backtrace) 0)

        ;; Frames are sequential
        (loop for i from 0 below (length backtrace)
              for frame in backtrace
              always (= (frame-number frame) i))

        ;; Expected functions appear in order
        (loop for expected-func in expected-call-chain
              for i from 0
              when (< i (length backtrace))
              always (search expected-func
                           (frame-function (nth i backtrace))
                           :test #'char-equal))

        ;; No duplicate frame numbers
        (= (length backtrace)
           (length (remove-duplicates backtrace
                                     :key #'frame-number)))))))
```

**Property Test Strategy**:

1. **Simple Error**:
   ```lisp
   (verify-backtrace-accuracy
     "(/ 1 0)"
     '("/" "EVAL"))
   ;; Stack should show division and eval context
   ```

2. **Nested Function Calls**:
   ```lisp
   (verify-backtrace-accuracy
     "(defun a () (/ 1 0))
      (defun b () (a))
      (defun c () (b))
      (c)"
     '("/" "A" "B" "C"))
   ;; Each function should appear in reverse call order
   ```

3. **Recursive Function**:
   ```lisp
   (verify-backtrace-accuracy
     "(defun factorial (n)
        (if (= n 0)
            (error \"base case\")
            (* n (factorial (1- n)))))
      (factorial 3)"
     '("ERROR" "FACTORIAL" "FACTORIAL" "FACTORIAL"))
   ;; Multiple FACTORIAL frames for recursion
   ```

4. **Lambda Calls**:
   ```lisp
   (verify-backtrace-accuracy
     "(funcall (lambda () (/ 1 0)))"
     '("/" "LAMBDA"))
   ;; Lambda should be identifiable
   ```

5. **Method Calls**:
   ```lisp
   (verify-backtrace-accuracy
     "(defmethod test-method ((x integer)) (/ x 0))
      (test-method 42)"
     '("/" "TEST-METHOD"))
   ;; Generic function dispatch visible
   ```

6. **Max Frames Limit**:
   ```lisp
   ;; Generate deep call stack (50 levels)
   (let ((deep-code (generate-deep-call-chain 50)))
     (evaluate-lisp deep-code)
     (let ((bt-20 (get-backtrace :max-frames 20))
           (bt-full (get-backtrace :max-frames 1000)))
       (and
         (= (length bt-20) 20)
         (>= (length bt-full) 50)
         ;; First 20 frames should match
         (equal (subseq bt-full 0 20) bt-20))))
   ```

**Edge Cases**:

- **Compiler Errors**: Backtrace during compilation
- **Reader Errors**: Stack during read phase
- **Stack Overflow**: Very deep recursion
- **Tail Call Optimization**: Optimized-away frames
- **Inline Functions**: Inlined calls might not appear
- **SBCL Internals**: Internal frames should be visible

**Implementation Requirements**:

```lisp
;; Capture backtrace at error point using SBCL introspection
(defun capture-backtrace-at-error (condition)
  (let ((frames '()))
    (sb-debug:map-backtrace
     (lambda (frame)
       (push (make-frame-info frame) frames)))
    (nreverse frames)))

(defun make-frame-info (frame)
  (list :number (sb-debug:frame-number frame)
        :function (sb-debug:frame-call frame)
        :source (sb-debug:frame-source-location frame)))
```

**Shrinking**: Find minimal code that produces inaccurate backtrace

## Related Properties

- **error-capture-completeness**: Backtrace is part of complete error capture
- **error-state-preservation**: Backtrace persists across tool calls
- **restart-information-completeness**: Restarts and backtrace both captured
