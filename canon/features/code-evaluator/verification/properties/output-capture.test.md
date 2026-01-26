---
type: verification
name: output-capture-property-test
source: properties/output-capture.md
level: property
tags:
  - property-based
  - output-streams
---

# Property Test: Output Capture

## Purpose

Verify that stdout, stderr, warnings, and return values are correctly captured and separated in all evaluation scenarios.

## Prerequisites

- Property-based testing framework
- Test session

## Implementation

### Generator: Random Output-Producing Code

```lisp
(defun generate-output-code ()
  "Generate code that produces various output types"
  (let ((generators
         (list
          ;; Stdout only
          (lambda () (format nil "(princ \"~A\") ~A" (random-string) (random 100)))

          ;; Stderr only
          (lambda () (format nil "(format *error-output* \"~A\") ~A" (random-string) (random 100)))

          ;; Warning only
          (lambda () (format nil "(warn \"~A\") ~A" (random-string) (random 100)))

          ;; Multiple outputs
          (lambda () (format nil "(princ \"~A\") (format *error-output* \"~A\") (warn \"~A\") ~A"
                            (random-string) (random-string) (random-string) (random 100)))

          ;; Just return value
          (lambda () (format nil "~A" (random-choice '("(+ 1 2)" "(list 1 2 3)" ":keyword")))))))

    (funcall (random-choice generators))))

(defun random-string ()
  "Generate random string"
  (format nil "test-~A" (random 1000)))

(defun random-choice (list)
  (nth (random (length list)) list))
```

### Property 1: All Output Types Captured

```lisp
(fiveam:test all-output-types-captured
  "All output types are captured and present"
  (let ((session (make-test-session)))
    (dotimes (trial 100)
      (let* ((code (generate-output-code))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Should succeed
        (fiveam:is (not (result-field result :isError)))

        ;; Should always have a result
        (fiveam:is (or (search "=>" text) (search "NIL" text)))))))
```

### Property 2: Stream Separation

```lisp
(fiveam:test stream-separation
  "Different output types are in separate sections"
  (let ((session (make-test-session)))
    ;; Test specific combinations
    (let* ((test-cases
            '(("(princ \"OUT\") 1" . ("[stdout]" "OUT"))
              ("(format *error-output* \"ERR\") 2" . ("[stderr]" "ERR"))
              ("(warn \"WRN\") 3" . ("[warnings]" "WRN"))
              ("(princ \"A\") (format *error-output* \"B\") :c" .
               ("[stdout]" "A" "[stderr]" "B" ":C"))))

      (dolist (test test-cases)
        (destructuring-bind (code . expected-markers) test
          (let* ((result (evaluate-in-session session code))
                 (text (result-content-text result)))
            (dolist (marker expected-markers)
              (fiveam:is (search marker text)
                        "Missing ~A in output of: ~A" marker code))))))))
```

### Property 3: Empty Sections Omitted

```lisp
(fiveam:test empty-sections-omitted
  "Sections with no output are not included"
  (let ((session (make-test-session)))
    (let* ((test-cases
            '(("(+ 1 2)" . ("[stdout]" "[stderr]" "[warnings]"))
              ("(princ \"x\") 1" . ("[stderr]" "[warnings]"))
              ("(format *error-output* \"y\") 2" . ("[stdout]" "[warnings]"))
              ("(warn \"z\") 3" . ("[stdout]" "[stderr]")))))

      (dolist (test test-cases)
        (destructuring-bind (code . should-not-have) test
          (let* ((result (evaluate-in-session session code))
                 (text (result-content-text result)))
            (dolist (section should-not-have)
              (fiveam:is (not (search section text))
                        "Section ~A should not be present for: ~A" section code))))))))
```

### Property 4: Multiple Values Formatted

```lisp
(fiveam:test multiple-values-formatted
  "Multiple return values are each shown with =>"
  (let ((session (make-test-session)))
    (let* ((test-cases
            '(("(floor 17 5)" . 2)
              ("(truncate 22 7)" . 2)
              ("(values 1 2 3)" . 3)
              ("(values)" . 0)))  ; No values

      (dolist (test test-cases)
        (destructuring-bind (code . expected-count) test
          (let* ((result (evaluate-in-session session code))
                 (text (result-content-text result))
                 (arrow-count (count-occurrences "=>" text)))
            (fiveam:is (= arrow-count expected-count)
                      "Expected ~A arrows for ~A, got ~A" expected-count code arrow-count)))))))
```

### Property 5: Output Order Preserved

```lisp
(fiveam:test output-order-preserved
  "Output within a stream appears in execution order"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((n (+ 3 (random 5)))
             (code (format nil "~{(princ \"~A \")~} nil"
                          (loop for i from 1 to n collect i)))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Numbers should appear in order
        (loop for i from 1 to (1- n)
              do (let ((pos-i (search (prin1-to-string i) text))
                      (pos-next (search (prin1-to-string (1+ i)) text)))
                   (fiveam:is (< pos-i pos-next)
                             "Numbers out of order in: ~A" code)))))))
```

### Property 6: Section Order Consistent

```lisp
(fiveam:test section-order-consistent
  "Sections appear in consistent order: stdout, stderr, warnings, result"
  (let ((session (make-test-session)))
    (let* ((code "(princ \"OUT\") (format *error-output* \"ERR\") (warn \"WRN\") :VAL")
           (result (evaluate-in-session session code))
           (text (result-content-text result)))

      ;; Find positions
      (let ((stdout-pos (search "[stdout]" text))
            (stderr-pos (search "[stderr]" text))
            (warnings-pos (search "[warnings]" text))
            (result-pos (search "=>" text)))

        ;; All should be present
        (fiveam:is stdout-pos)
        (fiveam:is stderr-pos)
        (fiveam:is warnings-pos)
        (fiveam:is result-pos)

        ;; Should be in order
        (fiveam:is (< stdout-pos stderr-pos))
        (fiveam:is (< stderr-pos warnings-pos))
        (fiveam:is (< warnings-pos result-pos))))))
```

### Property 7: Mixed Output Distinguishable

```lisp
(fiveam:test mixed-output-distinguishable
  "Can distinguish between different output types"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((stdout-marker (random-string))
             (stderr-marker (random-string))
             (warning-marker (random-string))
             (code (format nil "(princ \"~A\") (format *error-output* \"~A\") (warn \"~A\") :result"
                          stdout-marker stderr-marker warning-marker))
             (result (evaluate-in-session session code))
             (text (result-content-text result)))

        ;; Extract sections
        (let ((stdout-section (extract-section text "[stdout]" "[stderr]"))
              (stderr-section (extract-section text "[stderr]" "[warnings]"))
              (warnings-section (extract-section text "[warnings]" "=>")))

          ;; Each section should contain only its marker
          (fiveam:is (search stdout-marker stdout-section))
          (fiveam:is (not (search stderr-marker stdout-section)))
          (fiveam:is (not (search warning-marker stdout-section)))

          (fiveam:is (search stderr-marker stderr-section))
          (fiveam:is (not (search stdout-marker stderr-section)))

          (fiveam:is (search warning-marker warnings-section)))))))
```

### Property 8: Large Output Handling

```lisp
(fiveam:test large-output-handling
  "Large amounts of output are captured correctly"
  (let ((session (make-test-session)))
    (let* ((line-count 100)
           (code (format nil "(dotimes (i ~A) (princ i) (princ \" \")) :done" line-count))
           (result (evaluate-in-session session code))
           (text (result-content-text result)))

      ;; Should succeed
      (fiveam:is (not (result-field result :isError)))

      ;; Should have stdout section
      (fiveam:is (search "[stdout]" text))

      ;; Should have captured first and last numbers
      (fiveam:is (search "0" text))
      (fiveam:is (search (prin1-to-string (1- line-count)) text))

      ;; Should have result
      (fiveam:is (search "=> :DONE" text)))))
```

## Configuration

- Trials: 100 for normal runs
- Trials: 500 for thorough testing
- Shrinking: Enabled

## Helper Functions

```lisp
(defun extract-section (text start-marker &optional end-marker)
  "Extract text between markers"
  (let* ((start-pos (search start-marker text))
         (content-start (if start-pos (+ start-pos (length start-marker)) 0))
         (end-pos (if end-marker
                     (search end-marker text :start2 content-start)
                     (length text))))
    (subseq text content-start (or end-pos (length text)))))

(defun count-occurrences (substring string)
  "Count occurrences of substring in string"
  (loop with start = 0
        for pos = (search substring string :start2 start)
        while pos
        count pos
        do (setf start (1+ pos))))
```

## Assertions

For every trial:
1. All output types are captured
2. Different output types are in separate sections
3. Empty sections are omitted
4. Multiple return values are properly formatted
5. Output order is preserved within streams
6. Section order is consistent
7. Mixed output remains distinguishable

## Notes

- This test verifies INV-004 (Output Stream Separation)
- Tests all combinations of output types
- Verifies section markers and ordering
- Confirms large output is handled correctly
