---
type: verification
name: output-capture-scenario-test
source: scenarios/output-capture.md
level: scenario
tags:
  - integration
  - output-streams
---

# Scenario Test: Output Stream Capture

## Purpose

Verify that stdout, stderr, warnings, and return values are correctly captured and separated in evaluation responses.

## Prerequisites

- Initialized server with MCP session

## Setup

```lisp
(defvar *test-server* (cl-mcp-server:start-test-server))

(defun eval-code (id code)
  (send-request *test-server*
               `((:jsonrpc . "2.0")
                 (:id . ,id)
                 (:method . "tools/call")
                 (:params . ((:name . "evaluate-lisp")
                            (:arguments . ((:code . ,code))))))))
```

## Execution

### Scenario 1: Standard Output Capture

```lisp
(let* ((response (eval-code 1 "(format t \"Hello, World!~%\") 42"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))

  ;; Must have stdout section
  (assert (search "[stdout]" text))
  (assert (search "Hello, World!" text))

  ;; Must have result
  (assert (search "=> 42" text))

  ;; Stdout comes before result
  (assert (< (search "[stdout]" text) (search "=> 42" text))))
```

### Scenario 2: Multiple Print Statements

```lisp
(let* ((response (eval-code 2 "(print 1) (print 2) (print 3)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[stdout]" text))

  ;; All values printed (with spaces/newlines)
  (assert (search "1" text))
  (assert (search "2" text))
  (assert (search "3" text))

  ;; Result is last printed value
  (assert (search "=> 3" text)))
```

### Scenario 3: Error Output Capture

```lisp
(let* ((response (eval-code 3 "(format *error-output* \"Warning: something happened~%\") :ok"))
       (text (result-content-text response)))
  (assert (result-response-p response))

  ;; Must have stderr section
  (assert (search "[stderr]" text))
  (assert (search "Warning: something happened" text))

  ;; Must have result
  (assert (search "=> :OK" text))

  ;; Should NOT have stdout section
  (assert (not (search "[stdout]" text))))
```

### Scenario 4: Mixed Output Streams

```lisp
(let* ((response (eval-code 4 "(format t \"stdout line~%\") (format *error-output* \"stderr line~%\") :done"))
       (text (result-content-text response)))
  (assert (result-response-p response))

  ;; Both sections present
  (assert (search "[stdout]" text))
  (assert (search "stdout line" text))
  (assert (search "[stderr]" text))
  (assert (search "stderr line" text))

  ;; Result present
  (assert (search "=> :DONE" text))

  ;; Sections in correct order: stdout, stderr, result
  (let ((stdout-pos (search "[stdout]" text))
        (stderr-pos (search "[stderr]" text))
        (result-pos (search "=>" text)))
    (assert (< stdout-pos stderr-pos))
    (assert (< stderr-pos result-pos))))
```

### Scenario 5: Warning Capture

```lisp
(let* ((response (eval-code 5 "(warn \"This is a warning\") :completed"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))  ; Warnings don't make it an error

  ;; Must have warnings section
  (assert (search "[warnings]" text))
  (assert (search "WARNING" text))
  (assert (search "This is a warning" text))

  ;; Must have result (evaluation continues after warning)
  (assert (search "=> :COMPLETED" text)))
```

### Scenario 6: Multiple Warnings

```lisp
(let* ((response (eval-code 6 "(warn \"first\") (warn \"second\") (warn \"third\") :done"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[warnings]" text))

  ;; All warnings captured
  (assert (search "first" text))
  (assert (search "second" text))
  (assert (search "third" text))

  ;; Result still present
  (assert (search "=> :DONE" text)))
```

### Scenario 7: Clean Result (No Output)

```lisp
(let* ((response (eval-code 7 "(* 6 7)"))
       (text (result-content-text response)))
  (assert (result-response-p response))

  ;; Only result, no output sections
  (assert (not (search "[stdout]" text)))
  (assert (not (search "[stderr]" text)))
  (assert (not (search "[warnings]" text)))
  (assert (search "=> 42" text)))
```

### Scenario 8: All Output Types Together

```lisp
(let* ((code "(format t \"info~%\") (format *error-output* \"error~%\") (warn \"warning\") :result")
       (response (eval-code 8 code))
       (text (result-content-text response)))
  (assert (result-response-p response))

  ;; All sections present
  (assert (search "[stdout]" text))
  (assert (search "info" text))
  (assert (search "[stderr]" text))
  (assert (search "error" text))
  (assert (search "[warnings]" text))
  (assert (search "warning" text))
  (assert (search "=> :RESULT" text)))
```

## Verification

### Output Sections Are Distinguishable

```lisp
;; Verify we can parse different output types
(let* ((code "(format t \"OUT\") (format *error-output* \"ERR\") (warn \"WRN\") :VAL")
       (response (eval-code 100 code))
       (text (result-content-text response)))

  ;; Extract each section
  (let ((stdout-section (extract-section text "[stdout]"))
        (stderr-section (extract-section text "[stderr]"))
        (warnings-section (extract-section text "[warnings]"))
        (result-section (extract-section text "=>")))

    ;; Each section contains only its content
    (assert (search "OUT" stdout-section))
    (assert (not (search "ERR" stdout-section)))
    (assert (not (search "WRN" stdout-section)))

    (assert (search "ERR" stderr-section))
    (assert (not (search "OUT" stderr-section)))

    (assert (search "WRN" warnings-section))

    (assert (search ":VAL" result-section))))
```

### Empty Sections Omitted

```lisp
;; Test various combinations
(let* ((cases '(("(+ 1 2)" . ("[stdout]" "[stderr]" "[warnings]"))
                ("(format t \"x\") 1" . ("[stderr]" "[warnings]"))
                ("(format *error-output* \"y\") 2" . ("[stdout]" "[warnings]"))
                ("(warn \"z\") 3" . ("[stdout]" "[stderr]")))))
  (dolist (test-case cases)
    (let* ((code (car test-case))
           (should-not-have (cdr test-case))
           (response (eval-code (incf *test-id*) code))
           (text (result-content-text response)))
      ;; Verify omitted sections
      (dolist (section should-not-have)
        (assert (not (search section text))
                () "Section ~A should not be present in output of: ~A" section code)))))
```

### Verify INV-004: Output Stream Separation

```lisp
;; Property: All output types are distinguishable
(let* ((test-cases
        '(("stdout only" "(print 'x)" "[stdout]")
          ("stderr only" "(format *error-output* \"e\")" "[stderr]")
          ("warning only" "(warn \"w\") nil" "[warnings]")
          ("mixed" "(print 1) (warn \"w\") 2" ("[stdout]" "[warnings]" "=>")))))
  (dolist (test test-cases)
    (destructuring-bind (name code expected) test
      (let* ((response (eval-code (incf *test-id*) code))
             (text (result-content-text response)))
        (if (listp expected)
            (dolist (marker expected)
              (assert (search marker text) () "~A: Missing ~A" name marker))
            (assert (search expected text) () "~A: Missing ~A" name expected))))))
```

### Stream Ordering Preserved

```lisp
;; Within a stream, output order is preserved
(let* ((response (eval-code 200 "(princ \"A\") (princ \"B\") (princ \"C\") nil"))
       (text (result-content-text response)))
  (assert (< (search "A" text) (search "B" text)))
  (assert (< (search "B" text) (search "C" text))))
```

### Large Output Handling

```lisp
;; Generate substantial output
(let* ((response (eval-code 201 "(dotimes (i 100) (princ i) (princ \" \")) :done"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "[stdout]" text))
  (assert (search "0" text))
  (assert (search "99" text))
  (assert (search "=> :DONE" text)))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *test-server*)
```

## Notes

- This scenario verifies INV-004 (Output Stream Separation)
- Stdout, stderr, warnings, and return values are all distinguishable
- Empty sections are omitted from output
- Multiple values of the same type are captured
- Output order is preserved within each stream
- Section order is: [stdout], [stderr], [warnings], then result
