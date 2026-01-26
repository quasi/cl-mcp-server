---
type: verification
name: hot-spot-identification-property-test
source: properties/hot-spot-identification.md
level: property
tags:
  - property-based
  - profiling
  - hot-spots
---

# Property Test: Hot Spot Identification

## Purpose

Verify that profiling correctly identifies computational hot spots in code.

## Prerequisites

- Initialized MCP server
- Profiling tools available

## Implementation

### Property: Known Hot Spot Detected

```lisp
(deftest hot-spot-identification-known-hotspot ()
  "Profiling identifies intentional hot spot"
  (dotimes (i 10)
    (let* ((code "(defun hotspot () (loop repeat 10000000 sum (random 100)))
                  (defun light () (+ 1 2))
                  (progn (hotspot) (light) (hotspot))")
           (response (call-tool *test-server* "profile-code"
                               `(("code" . ,code)
                                 ("mode" . "cpu"))))
           (content (result-content response)))

      ;; HOTSPOT should be top function
      (is (search "HOTSPOT" content :test #'char-equal)
          "HOTSPOT function not in profile")

      ;; Should have high percentage
      (let ((hotspot-line (find-line-with "HOTSPOT" content)))
        (when hotspot-line
          (is (has-high-percentage hotspot-line 50)
              "HOTSPOT doesn't have high sample percentage"))))))

(defun find-line-with (substring text)
  "Find line containing substring"
  (find-if (lambda (line) (search substring line :test #'char-equal))
           (split-lines text)))

(defun has-high-percentage (line threshold)
  "Check if line contains percentage >= threshold"
  (let ((pct-pos (position #\% line)))
    (when pct-pos
      (let ((num-text (extract-number-before line pct-pos)))
        (>= (parse-float num-text) threshold)))))

(defun split-lines (text)
  "Split text into lines"
  (loop with start = 0
        for end = (position #\Newline text :start start)
        while end
        collect (subseq text start end)
        do (setf start (1+ end))
        finally (when (< start (length text))
                  (collect (subseq text start)))))
```

### Property: Relative Hot Spots

```lisp
(deftest hot-spot-identification-relative ()
  "More expensive function has more samples"
  (dotimes (i 5)
    (let* ((code "(defun expensive () (loop repeat 10000000 sum (random 100)))
                  (defun cheap () (+ 1 2 3))
                  (dotimes (i 10) (expensive) (cheap))")
           (response (call-tool *test-server* "profile-code"
                               `(("code" . ,code)
                                 ("mode" . "cpu"))))
           (content (result-content response))
           (expensive-pct (extract-function-percentage content "EXPENSIVE"))
           (cheap-pct (extract-function-percentage content "CHEAP")))

      ;; EXPENSIVE should have much higher percentage
      (is (> expensive-pct cheap-pct)
          "EXPENSIVE (~A%) not greater than CHEAP (~A%)"
          expensive-pct cheap-pct))))

(defun extract-function-percentage (text function-name)
  "Extract percentage for function from profile output"
  (let ((line (find-line-with function-name text)))
    (if line
        (or (find-percentage-in-line line) 0.0)
        0.0)))

(defun find-percentage-in-line (line)
  "Find first percentage number in line"
  (let ((pct-pos (position #\% line)))
    (when pct-pos
      (let ((num-text (extract-number-before line pct-pos)))
        (parse-float num-text)))))
```

### Property: Allocation Hot Spots

```lisp
(deftest hot-spot-identification-allocation ()
  "Allocation profiling identifies allocation hot spots"
  (dotimes (i 5)
    (let* ((code "(defun allocate-lots () (loop repeat 10000 collect (make-list 100)))
                  (defun allocate-little () (cons 1 2))
                  (progn (allocate-lots) (allocate-little))")
           (response (call-tool *test-server* "profile-code"
                               `(("code" . ,code)
                                 ("mode" . "alloc"))))
           (content (result-content response)))

      ;; ALLOCATE-LOTS should be top allocator
      (is (search "ALLOCATE-LOTS" content :test #'char-equal)
          "ALLOCATE-LOTS not in allocation profile")

      ;; Should show significant allocation
      (let ((alloc-line (find-line-with "ALLOCATE-LOTS" content)))
        (when alloc-line
          (is (or (search "MB" alloc-line)
                  (search "KB" alloc-line))
              "ALLOCATE-LOTS line doesn't show allocation size"))))))
```

### Property: Top-N Coverage

```lisp
(deftest hot-spot-identification-top-n ()
  "Top functions account for majority of samples"
  (dotimes (i 3)
    (let* ((code "(defun f1 () (loop repeat 5000000 sum (random 100)))
                  (defun f2 () (loop repeat 3000000 sum (random 100)))
                  (defun f3 () (loop repeat 1000000 sum (random 100)))
                  (progn (f1) (f2) (f3))")
           (response (call-tool *test-server* "profile-code"
                               `(("code" . ,code)
                                 ("mode" . "cpu"))))
           (content (result-content response))
           (f1-pct (extract-function-percentage content "F1"))
           (f2-pct (extract-function-percentage content "F2"))
           (f3-pct (extract-function-percentage content "F3"))
           (top3-total (+ f1-pct f2-pct f3-pct)))

      ;; Top 3 should cover >50% of samples
      (is (> top3-total 50)
          "Top 3 functions only ~A% of samples" top3-total))))
```

## Configuration

- Examples: 5-10 per test
- Use code with clear hot spots
- Verify both CPU and allocation modes

## Helper Functions

```lisp
(defun parse-float (string)
  "Parse floating point number from string"
  (let ((cleaned (string-trim '(#\Space #\Tab) string)))
    (ignore-errors (read-from-string cleaned))))

(defun extract-number-before (text pos)
  "Extract number before position"
  (let ((start (or (position-if-not #'digit-char-p text
                                     :end pos :from-end t)
                   -1)))
    (subseq text (1+ start) pos)))
```

## Notes

- Hot spot detection depends on sufficient samples
- Known hot spots should consistently appear in top results
- Relative costs should be reflected in sample percentages
- Different profiling modes highlight different hot spots
