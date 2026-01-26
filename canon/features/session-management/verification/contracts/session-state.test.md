---
type: verification
name: session-state-contract-test
source: contracts/session-state.md
level: contract
tags:
  - smoke
  - session
---

# Contract Test: Session State

## Purpose

Verify the session state contract defining persistent and non-persistent state across evaluations.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test Cases

### Persistent State: Functions

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(defun test-square (x) (* x x))"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  ;; Success response
  (assert (result-response-p response))
  (assert (not (is-error response))))

;; Use function in next evaluation
(let ((response2 (send-request *server*
                   '((:jsonrpc . "2.0")
                     (:id . 2)
                     (:method . "tools/call")
                     (:params . ((:name . "evaluate-lisp")
                                (:arguments . ((:code . "(test-square 7)")))))))))
  (assert (result-response-p response2))
  (assert (not (is-error response2)))
  (assert (search "49" (extract-result-text response2))))
```

### Persistent State: Variables

**Input**:
```lisp
;; Define variable
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 3)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defvar *test-counter* 0)")))))))))
  (assert (result-response-p r1)))

;; Modify variable
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 4)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(incf *test-counter*)")))))))))
  (assert (search "1" (extract-result-text r2))))

;; Read variable again
(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 5)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "*test-counter*")))))))))
  (assert (search "1" (extract-result-text r3))))
```

### Persistent State: Package Context

**Input**:
```lisp
;; Create package
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 6)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defpackage :test-pkg (:use :cl))")))))))))
  (assert (result-response-p r1)))

;; Switch package
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 7)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(in-package :test-pkg)")))))))))
  (assert (result-response-p r2)))

;; Verify package persists
(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 8)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(package-name *package*)")))))))))
  (assert (search "TEST-PKG" (extract-result-text r3))))
```

### Non-Persistent State: Let Bindings

**Input**:
```lisp
;; Let binding in one evaluation
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 9)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(let ((x 42)) x)")))))))))
  (assert (search "42" (extract-result-text r1))))

;; x should be unbound in next evaluation
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "x")))))))))
  (assert (is-error r2))
  (assert (search "UNBOUND-VARIABLE" (extract-result-text r2) :test #'char-equal)))
```

### Tool: list-definitions

**Input**:
```json
{"jsonrpc":"2.0","id":11,"method":"tools/call","params":{"name":"list-definitions","arguments":{}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (let ((text (extract-result-text response)))
    ;; Should list previously defined function
    (assert (search "TEST-SQUARE" text :test #'char-equal))
    ;; Should list previously defined variable
    (assert (search "*TEST-COUNTER*" text :test #'char-equal))))
```

### Tool: reset-session

**Input**:
```lisp
;; Reset session
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 12)
              (:method . "tools/call")
              (:params . ((:name . "reset-session")
                         (:arguments . ())))))))
  (assert (result-response-p r1))
  (assert (search "reset" (extract-result-text r1) :test #'char-equal)))

;; Verify function is gone
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 13)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(test-square 5)")))))))))
  (assert (is-error r2))
  (assert (search "UNDEFINED-FUNCTION" (extract-result-text r2) :test #'char-equal)))

;; Verify variable is gone
(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 14)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "*test-counter*")))))))))
  (assert (is-error r3))
  (assert (search "UNBOUND-VARIABLE" (extract-result-text r3) :test #'char-equal)))

;; Verify back in CL-USER
(let ((r4 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 15)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(package-name *package*)")))))))))
  (assert (search "CL-USER" (extract-result-text r4))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Functions, variables, macros, classes must persist across evaluations
- Package context must persist
- Let bindings must NOT persist
- list-definitions must show all user-defined symbols
- reset-session must clear all user state
