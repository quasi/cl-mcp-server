---
type: verification
name: evaluation-errors-scenario-test
source: scenarios/evaluation-errors.md
level: scenario
tags:
  - error-handling
  - evaluation
---

# Scenario Test: Evaluation Error Handling

## Purpose

Verify the server handles various runtime errors gracefully and provides useful error information.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test: Comprehensive Error Handling

### Case 1: Undefined Function

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 1)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(nonexistent-function 1 2 3)")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "UNDEFINED-FUNCTION" text :test #'char-equal))
    (assert (search "NONEXISTENT-FUNCTION" text :test #'char-equal))))
```

### Case 2: Unbound Variable

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 2)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(+ x 1)")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "UNBOUND-VARIABLE" text :test #'char-equal))
    (assert (search "X" text))))
```

### Case 3: Type Error

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 3)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(+ 1 \"hello\")")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "TYPE-ERROR" text :test #'char-equal))))
```

### Case 4: Division by Zero

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 4)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(/ 1 0)")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "DIVISION-BY-ZERO" text :test #'char-equal))))
```

### Case 5: Index Out of Bounds

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 5)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(aref #(1 2 3) 10)")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; SBCL-specific error type acceptable
    (assert (or (search "INDEX" text :test #'char-equal)
                (search "ERROR" text :test #'char-equal)))))
```

### Case 6: Reader Error

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 6)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(+ 1 2")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (or (search "END-OF-FILE" text :test #'char-equal)
                (search "READER-ERROR" text :test #'char-equal)))))
```

### Case 7: Package Error

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 7)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(in-package :totally-nonexistent)")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "PACKAGE" text :test #'char-equal))))
```

### Case 8: User-Signaled Error

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 8)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(error \"Custom error message\")")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "Custom error message" text))))
```

### Case 9: Nested Function Error

```lisp
;; Define functions that call each other
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 9)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun level3 () (/ 10 0))")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun level2 () (level3))")))))))))
  (assert (result-response-p r2)))

(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 11)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun level1 () (level2))")))))))))
  (assert (result-response-p r3)))

;; Trigger nested error
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 12)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(level1)")))))))))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; Should show backtrace with multiple levels
    (assert (search "Backtrace" text :test #'char-equal))
    (assert (search "LEVEL3" text :test #'char-equal))))
```

## Verification: Server Stability After Errors

```lisp
(defun verify-server-operational ()
  "Verify server still works after error"
  (let ((response (send-request *server*
                    '((:jsonrpc . "2.0")
                      (:id . 999)
                      (:method . "tools/call")
                      (:params . ((:name . "evaluate-lisp")
                                 (:arguments . ((:code . "(+ 1 2)")))))))))
    (assert (result-response-p response))
    (assert (not (is-error response)))
    (assert (search "3" (extract-result-text response)))))

;; Test after each major error type
(verify-server-operational)
```

## Verification: Session State Preserved

```lisp
;; Define something before errors
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 100)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defvar *test-persistence* 42)")))))))))
  (assert (result-response-p response)))

;; Cause several errors
(send-request *server* '((:jsonrpc . "2.0") (:id . 101) (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp") (:arguments . ((:code . "(unknown-fn)")))))))
(send-request *server* '((:jsonrpc . "2.0") (:id . 102) (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp") (:arguments . ((:code . "(/ 1 0)")))))))

;; Verify original definition still exists
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 103)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "*test-persistence*")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (assert (search "42" (extract-result-text response))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Server must handle all common error types
- Each error must include condition type and message
- Backtraces must show call stack
- Server must remain operational after errors
- Session state must not be corrupted by errors
- Error responses must have `isError: true`
- Error information must be useful for debugging
