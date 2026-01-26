---
type: verification
name: condition-report-contract-test
source: contracts/condition-report.md
level: contract
tags:
  - smoke
  - error-handling
---

# Contract Test: Condition Report

## Purpose

Verify the condition report contract defining how errors are captured and formatted.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test Cases

### Undefined Function Error

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(nonexistent-fn 1 2 3)"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; Must include error type
    (assert (search "UNDEFINED-FUNCTION" text :test #'char-equal))
    ;; Must include function name
    (assert (search "NONEXISTENT-FN" text :test #'char-equal))
    ;; Must include backtrace
    (assert (search "Backtrace" text :test #'char-equal))
    ;; Must have ERROR marker
    (assert (search "[ERROR]" text :test #'char-equal))))
```

### Unbound Variable Error

**Input**:
```json
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(+ unknown-var 1)"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "UNBOUND-VARIABLE" text :test #'char-equal))
    (assert (search "UNKNOWN-VAR" text :test #'char-equal))
    (assert (search "Backtrace" text :test #'char-equal))))
```

### Type Error

**Input**:
```json
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(+ 1 \"string\")"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "TYPE-ERROR" text :test #'char-equal))
    ;; Should mention the invalid type or value
    (assert (or (search "string" text :test #'char-equal)
                (search "NUMBER" text)))))
```

### Division By Zero

**Input**:
```json
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(/ 1 0)"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "DIVISION-BY-ZERO" text :test #'char-equal))
    (assert (search "Backtrace" text :test #'char-equal))))
```

### Reader Error (Syntax)

**Input**:
```json
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(+ 1 2"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; Should indicate reader/parse error
    (assert (or (search "END-OF-FILE" text :test #'char-equal)
                (search "READER-ERROR" text :test #'char-equal)))
    (assert (search "Backtrace" text :test #'char-equal))))
```

### Package Error

**Input**:
```json
{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(in-package :nonexistent-package)"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    (assert (search "PACKAGE" text :test #'char-equal))
    (assert (search "NONEXISTENT-PACKAGE" text :test #'char-equal))))
```

### Warning Capture (Not an Error)

**Input**:
```lisp
;; Code that produces warnings but succeeds
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 7)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun test-warn () (warn \"Test warning\") 42)")))))))))
  (assert (result-response-p response)))

;; Call it to trigger warning
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 8)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(test-warn)")))))))))
  (assert (result-response-p response))
  ;; Should NOT be marked as error
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should include warning
    (assert (search "warning" text :test #'char-equal))
    ;; Should include result
    (assert (search "42" text))))
```

### Error with Output

**Input**:
```lisp
;; Code that prints then errors
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 9)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(progn (format t \"Processing...~%\") (error \"Failed\"))")))))))))
  (assert (result-response-p response))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; Should include both output and error
    (assert (search "Processing" text :test #'char-equal))
    (assert (search "ERROR" text :test #'char-equal))
    (assert (search "Failed" text))))
```

### Backtrace Format

**Input**:
```lisp
;; Define nested functions to get multi-frame backtrace
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun inner-fn () (/ 1 0))")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 11)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun outer-fn () (inner-fn))")))))))))
  (assert (result-response-p r2)))

;; Trigger error with stack
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 12)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(outer-fn)")))))))))
  (assert (is-error response))
  (let ((text (extract-result-text response)))
    ;; Should show both functions in backtrace
    (assert (search "INNER-FN" text :test #'char-equal))
    (assert (search "OUTER-FN" text :test #'char-equal))
    ;; Backtrace should be structured (numbered frames)
    (assert (or (search "0:" text)
                (search "1:" text)))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Error responses must have `isError: true`
- Error type (condition class name) must be included
- Error message must be human-readable
- Backtrace must be included when available
- Warnings should NOT set `isError: true`
- Output before error must be preserved
- Server must remain operational after errors
