---
type: verification
name: backtrace-accuracy-property-test
source: properties/backtrace-accuracy.md
level: property
tags:
  - property-based
  - backtrace
  - accuracy
---

# Property Test: Backtrace Accuracy

## Purpose

Verify that backtraces accurately reflect the actual call chain at the point of error.

## Prerequisites

- Initialized MCP server
- Error-intelligence tools available

## Implementation

### Property: Call Chain Preserved

```lisp
(deftest backtrace-call-chain ()
  "Backtrace shows actual call sequence"
  (dotimes (depth 5)
    (let* ((functions (generate-call-chain (+ depth 2)))
           (code (format-call-chain functions))
           ;; Trigger error
           (_ (call-tool *test-server* "evaluate-lisp" `(("code" . ,code))))
           ;; Get backtrace
           (response (call-tool *test-server* "get-backtrace" '()))
           (content (result-content response)))

      ;; All function names should appear in backtrace
      (dolist (func functions)
        (is (search func content :test #'char-equal)
            "Function ~A missing from backtrace for chain: ~A"
            func functions)))))

(defun generate-call-chain (depth)
  "Generate list of function names"
  (loop for i from 1 to depth
        collect (format nil "FUNC-~A" i)))

(defun format-call-chain (functions)
  "Generate nested function call code"
  (let ((error-form "(/ 1 0)"))
    (reduce (lambda (inner fname)
              (format nil "(defun ~A () ~A)" fname inner))
            (reverse functions)
            :initial-value error-form
            :from-end t)))
```

### Property: Frame Order

```lisp
(deftest backtrace-frame-order ()
  "Frames appear in correct order (innermost to outermost)"
  (let* ((code "(defun a () (b)) (defun b () (c)) (defun c () (/ 1 0)) (a)")
         (_ (call-tool *test-server* "evaluate-lisp" `(("code" . ,code))))
         (response (call-tool *test-server* "get-backtrace" '()))
         (content (result-content response))
         (pos-div (search "/" content))
         (pos-c (search "C" content :start2 pos-div))
         (pos-b (search "B" content :start2 pos-c))
         (pos-a (search "A" content :start2 pos-b)))

    ;; Order should be: / → C → B → A
    (is (and pos-div pos-c pos-b pos-a)
        "Not all functions found in backtrace")
    (is (< pos-div pos-c pos-b pos-a)
        "Frame order incorrect: / before C before B before A")))
```

### Property: Recursive Calls Shown

```lisp
(deftest backtrace-recursion ()
  "Recursive calls appear multiple times in backtrace"
  (let* ((code "(defun factorial (n) (if (<= n 0) (error \"done\") (* n (factorial (- n 1))))) (factorial 5)")
         (_ (call-tool *test-server* "evaluate-lisp" `(("code" . ,code))))
         (response (call-tool *test-server* "get-backtrace" '(("max-frames" . 20))))
         (content (result-content response))
         (factorial-count (count-occurrences "FACTORIAL" content)))

    ;; Should show multiple FACTORIAL frames
    (is (>= factorial-count 2)
        "Expected multiple recursive frames, got ~A" factorial-count)))

(defun count-occurrences (substring text)
  (let ((count 0)
        (pos 0))
    (loop while (setf pos (search substring text :start2 pos :test #'char-equal))
          do (incf count) (incf pos))
    count))
```

### Property: Arguments Captured

```lisp
(deftest backtrace-arguments ()
  "Function arguments shown in backtrace (when available)"
  (let* ((code "(defun process (x y) (/ x y)) (process 10 0)")
         (_ (call-tool *test-server* "evaluate-lisp" `(("code" . ,code))))
         (response (call-tool *test-server* "get-backtrace" '()))
         (content (result-content response)))

    ;; Should show PROCESS
    (is (search "PROCESS" content :test #'char-equal)
        "PROCESS function not in backtrace")

    ;; May show arguments (implementation-dependent)
    (when (or (search "10" content) (search "0" content))
      (pass "Arguments shown in backtrace"))))
```

## Configuration

- Examples: 20-50 per test
- Verify both shallow and deep call stacks
- Test recursive and iterative patterns

## Notes

- Backtrace format may vary by implementation
- Argument capture is optional but desirable
- Frame order must be consistent (innermost first or outermost first)
- Recursive calls should show multiple frames
