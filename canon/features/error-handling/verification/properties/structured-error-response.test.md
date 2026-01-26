---
type: verification
name: structured-error-response-property-test
source: properties/structured-error-response.md
level: property
tags:
  - property-based
  - error-handling
  - format
---

# Property Test: Structured Error Response

## Purpose

Verify that all error responses follow the structured format contract.

## Prerequisites

- Initialized server
- Property-based testing framework

## Implementation

### Property: Error Response Format

```lisp
(test error-response-format-property
  "All error responses must follow the structured format"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 100)
           (let* ((error-code (generate-error-code))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,trial)
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,error-code)))))))))

             (when (is-error response)
               (let ((text (extract-result-text response)))
                 ;; Format: [ERROR] TYPE\nmessage\n\n[Backtrace]\n...

                 ;; Must start with [ERROR]
                 (is (search "[ERROR]" text :test #'char-equal)
                     "Must have [ERROR] marker")

                 ;; Must have condition type on first line
                 (let* ((first-line-end (or (position #\Newline text) (length text)))
                        (first-line (subseq text 0 first-line-end)))
                   (is (search "[ERROR]" first-line :test #'char-equal)
                       "First line should have [ERROR] marker")
                   (is (> (length first-line) 10)
                       "First line should include condition type"))

                 ;; Parse structure
                 (let ((has-error-marker (search "[ERROR]" text :test #'char-equal))
                       (has-backtrace (search "Backtrace" text :test #'char-equal))
                       (has-newlines (find #\Newline text)))
                   (is has-error-marker "Must have [ERROR] marker")
                   (is has-newlines "Must have multi-line structure"))))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: isError Flag Consistency

```lisp
(test is-error-flag-consistency-property
  "isError flag must be true for all errors, false for successes"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (progn
           ;; Test errors
           (dotimes (trial 50)
             (let* ((error-code (generate-error-code))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . ,trial)
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,error-code)))))))))
               (is (result-response-p response))
               (is (is-error response)
                   "Error code should set isError=true: ~A" error-code)
               (is (search "[ERROR]" (extract-result-text response) :test #'char-equal)
                   "Error text should have [ERROR] marker: ~A" error-code)))

           ;; Test successes
           (dotimes (trial 50)
             (let* ((success-code (format nil "(+ ~A ~A)" (random 100) (random 100)))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . ,(+ 50 trial))
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,success-code)))))))))
               (is (result-response-p response))
               (is (not (is-error response))
                   "Success code should set isError=false: ~A" success-code)
               (is (not (search "[ERROR]" (extract-result-text response) :test #'char-equal))
                   "Success text should not have [ERROR] marker: ~A" success-code))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Condition Type Preservation

```lisp
(test condition-type-preservation-property
  "Specific error types must be reported in error responses"
  (let ((server (make-initialized-test-server))
        (error-specs '(("(undefined-fn)" "UNDEFINED-FUNCTION")
                       ("(+ unknown-var 1)" "UNBOUND-VARIABLE")
                       ("(/ 1 0)" "DIVISION-BY-ZERO")
                       ("(+ 1 \"x\")" "TYPE-ERROR")
                       ("(+ 1 2" "END-OF-FILE")
                       ("(in-package :no-pkg)" "PACKAGE"))))
    (unwind-protect
         (dolist (spec error-specs)
           (let* ((code (first spec))
                  (expected-type (second spec))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . 1)
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,code)))))))))
             (is (is-error response)
                 "Code ~A should produce error" code)
             (let ((text (extract-result-text response)))
               (is (search expected-type text :test #'char-equal)
                   "Error text for ~A should include condition type ~A" code expected-type))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Backtrace Presence

```lisp
(test backtrace-presence-property
  "Most errors should include backtrace information"
  (let ((server (make-initialized-test-server))
        (backtrace-count 0)
        (total-errors 0))
    (unwind-protect
         (progn
           (dotimes (trial 100)
             (let* ((error-code (generate-error-code))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . ,trial)
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,error-code)))))))))
               (when (is-error response)
                 (incf total-errors)
                 (let ((text (extract-result-text response)))
                   (when (search "Backtrace" text :test #'char-equal)
                     (incf backtrace-count))))))

           ;; Most errors should have backtraces
           (is (> backtrace-count (* 0.7 total-errors))
               "At least 70% of errors should include backtrace (got ~A/~A)"
               backtrace-count total-errors))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Warning vs Error Distinction

```lisp
(test warning-vs-error-distinction-property
  "Warnings must not set isError flag, errors must"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (progn
           ;; Define function that warns
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 1)
                               (:method . "tools/call")
                               (:params . ((:name . "evaluate-lisp")
                                          (:arguments . ((:code . "(defun warn-fn () (warn \"test\") 42)")))))))))
             (is (result-response-p response)))

           ;; Call it multiple times - should succeed with warnings
           (dotimes (trial 20)
             (let ((response (send-request server
                               '((:jsonrpc . "2.0")
                                 (:id . 2)
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . "(warn-fn)")))))))))
               (is (result-response-p response))
               (is (not (is-error response))
                   "Warning should not set isError flag in trial ~A" trial)
               (let ((text (extract-result-text response)))
                 ;; Should have result
                 (is (search "42" text)
                     "Should have result value")
                 ;; May have warning marker
                 (when (search "warn" text :test #'char-equal)
                   (is (not (search "[ERROR]" text :test #'char-equal))
                       "Warning should not use [ERROR] marker")))))

           ;; Actual error should set flag
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 3)
                               (:method . "tools/call")
                               (:params . ((:name . "evaluate-lisp")
                                          (:arguments . ((:code . "(error \"test\")"))))) ))))
             (is (is-error response)
                 "Error should set isError flag")))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Output Preservation in Errors

```lisp
(test output-preservation-in-errors-property
  "Output before error must be included in error response"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 30)
           (let* ((marker (format nil "MARKER-~A" trial))
                  (code (format nil "(progn (format t \"~A~~%\") (/ 1 0))" marker))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,trial)
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,code)))))))))
             (is (is-error response)
                 "Code should error")
             (let ((text (extract-result-text response)))
               ;; Should include both output and error
               (is (search marker text :test #'char-equal)
                   "Should preserve output marker ~A" marker)
               (is (search "ERROR" text :test #'char-equal)
                   "Should include error information"))))
      (cl-mcp-server:stop-test-server server))))
```

## Configuration

- Trials: 100 for normal runs
- Trials: 500+ for thorough testing

## Assertions

For every error response:
1. Must have `isError: true` in result
2. Must include `[ERROR]` marker in text
3. Must include condition type name (uppercase identifier)
4. Must include human-readable message
5. Should include backtrace (>70% of cases)
6. Must preserve output that occurred before error
7. Warnings must NOT set `isError: true`

## Notes

- Validates the contract for error response structure
- Ensures consistency across all error types
- Verifies distinction between warnings and errors
- Tests that diagnostic information is always present
