---
type: verification
name: error-capture-stability-property-test
source: properties/error-capture-stability.md
level: property
tags:
  - property-based
  - error-handling
  - stability
---

# Property Test: Error Capture Stability

## Purpose

Verify that error capture mechanism is stable and doesn't crash the server, regardless of error type.

## Prerequisites

- Initialized server
- Property-based testing framework

## Implementation

### Generator: Random Error-Producing Code

```lisp
(defun generate-error-code ()
  "Generate code that will produce various errors"
  (random-choice
   (list
    ;; Undefined function
    (format nil "(~A ~A)" (gensym "UNDEF") (random 100))
    ;; Unbound variable
    (format nil "(+ ~A 1)" (gensym "VAR"))
    ;; Type error
    "(+ 1 \"string\")"
    "(car 42)"
    "(length 123)"
    ;; Division by zero
    "(/ 1 0)"
    "(/ 100 (* 0 5))"
    ;; Reader errors
    "(+ 1 2"
    ")"
    "(defun"
    ;; Index errors
    "(aref #() 0)"
    "(elt '() 5)"
    ;; User errors
    "(error \"Test error\")"
    "(cerror \"Continue\" \"Test condition\")"
    ;; Package errors
    "(in-package :nonexistent-package-xyz)"
    "(find-symbol \"X\" :no-such-package)")))
```

### Property: All Errors Are Captured

```lisp
(test error-capture-completeness-property
  "All errors must be captured and returned as error responses"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 200)
           (let* ((error-code (generate-error-code))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,trial)
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,error-code)))))))))

             ;; Must get a valid response
             (is (result-response-p response)
                 "Should get result response for code: ~A" error-code)

             ;; Must be marked as error
             (is (is-error response)
                 "Should be marked as error for code: ~A" error-code)

             ;; Must have error marker in text
             (let ((text (extract-result-text response)))
               (is (search "[ERROR]" text :test #'char-equal)
                   "Should have [ERROR] marker for code: ~A" error-code)

               ;; Must have some condition type
               (is (> (length text) 20)
                   "Error text should be non-trivial for code: ~A" error-code))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Server Survives All Errors

```lisp
(test server-survives-errors-property
  "Server must remain operational after any error"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 100)
           ;; Generate and execute error
           (let* ((error-code (generate-error-code))
                  (error-response (send-request server
                                    `((:jsonrpc . "2.0")
                                      (:id . ,(* trial 2))
                                      (:method . "tools/call")
                                      (:params . ((:name . "evaluate-lisp")
                                                 (:arguments . ((:code . ,error-code)))))))))
             (is (result-response-p error-response)))

           ;; Verify server still works
           (let ((test-response (send-request server
                                  `((:jsonrpc . "2.0")
                                    (:id . ,(1+ (* trial 2)))
                                    (:method . "tools/call")
                                    (:params . ((:name . "evaluate-lisp")
                                               (:arguments . ((:code . "(+ 1 1)")))))))))
             (is (result-response-p test-response)
                 "Server should respond after error in trial ~A" trial)
             (is (not (is-error test-response))
                 "Simple evaluation should work after error in trial ~A" trial)
             (is (search "2" (extract-result-text test-response))
                 "Should get correct result after error in trial ~A" trial)))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Session State Unaffected by Errors

```lisp
(test errors-dont-corrupt-state-property
  "Errors must not corrupt session state"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (cycle 20)
           ;; Define something
           (let* ((fn-name (format nil "TEST-FN-~A" cycle))
                  (def-code (format nil "(defun ~A () ~A)" fn-name cycle)))
             (let ((response (send-request server
                               `((:jsonrpc . "2.0")
                                 (:id . ,(* cycle 4))
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . ,def-code)))))))))
               (is (result-response-p response))))

           ;; Cause an error
           (let ((error-code (generate-error-code)))
             (send-request server
               `((:jsonrpc . "2.0")
                 (:id . ,(1+ (* cycle 4)))
                 (:method . "tools/call")
                 (:params . ((:name . "evaluate-lisp")
                            (:arguments . ((:code . ,error-code))))))))

           ;; Verify definition still works
           (let* ((fn-name (format nil "TEST-FN-~A" cycle))
                  (call-code (format nil "(~A)" fn-name))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,(+ 2 (* cycle 4)))
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,call-code)))))))))
             (is (result-response-p response)
                 "Should be able to call ~A after error" fn-name)
             (is (not (is-error response))
                 "Function ~A should still work after error" fn-name)
             (is (search (format nil "~A" cycle) (extract-result-text response))
                 "Function ~A should return correct value after error" fn-name)))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Error Information Quality

```lisp
(test error-information-quality-property
  "All error responses must include condition type and message"
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
                 ;; Must have error marker
                 (is (search "[ERROR]" text :test #'char-equal)
                     "Should have [ERROR] marker")

                 ;; Must have condition type (uppercase identifier after [ERROR])
                 (is (position-if #'alpha-char-p text
                                  :start (+ 7 (search "[ERROR]" text :test #'char-equal)))
                     "Should have condition type after [ERROR]")

                 ;; Must have backtrace section (most errors)
                 (is (or (search "Backtrace" text :test #'char-equal)
                         (search "reader" text :test #'char-equal))  ; reader errors may differ
                     "Should usually have Backtrace section")

                 ;; Must be more than just the marker
                 (is (> (length text) 50)
                     "Error message should have substantial content")))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Consecutive Errors

```lisp
(test consecutive-errors-property
  "Server must handle consecutive errors without degradation"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 50)
           ;; Send 10 consecutive errors
           (dotimes (i 10)
             (let* ((error-code (generate-error-code))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . ,(+ i (* trial 11)))
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,error-code)))))))))
               (is (result-response-p response)
                   "Should handle consecutive error ~A in trial ~A" i trial)
               (is (is-error response)
                   "Should mark as error ~A in trial ~A" i trial)))

           ;; Verify still operational
           (let ((test-response (send-request server
                                  `((:jsonrpc . "2.0")
                                    (:id . ,(+ 10 (* trial 11)))
                                    (:method . "tools/call")
                                    (:params . ((:name . "evaluate-lisp")
                                               (:arguments . ((:code . "(list 1 2 3)")))))))))
             (is (result-response-p test-response)
                 "Should work after 10 consecutive errors in trial ~A" trial)
             (is (not (is-error test-response))
                 "Should successfully evaluate after errors in trial ~A" trial)))
      (cl-mcp-server:stop-test-server server))))
```

## Configuration

- Trials: 100-200 for normal runs
- Trials: 1000+ for stress testing

## Assertions

For every error-producing code:
1. Server must return a valid response (not crash)
2. Response must be marked as error (`isError: true`)
3. Response must include condition type
4. Response must include error message
5. Server must remain operational after error
6. Session state must not be corrupted

## Notes

- Tests worst-case scenarios: consecutive errors, random error types
- Verifies error handling doesn't introduce memory leaks or state corruption
- Ensures error information is always useful
- Critical for server stability in production use
