---
type: verification
name: state-persistence-scenario-test
source: scenarios/state-persistence.md
level: scenario
tags:
  - session
  - persistence
---

# Scenario Test: State Persistence

## Purpose

Verify state persists across multiple evaluations in realistic workflows.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test: Incremental Development Workflow

### Step 1: Define Helper Functions

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 1)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun square (x) (* x x))")))))))))
  (assert (result-response-p response)))

(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 2)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun cube (x) (* x x x))")))))))))
  (assert (result-response-p response)))
```

### Step 2: Build On Helpers

```lisp
;; Define function using previous definitions
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 3)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun sum-of-squares (a b) (+ (square a) (square b)))")))))))))
  (assert (result-response-p response)))

;; Test it
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 4)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(sum-of-squares 3 4))")))))))))
  (assert (result-response-p response))
  (assert (search "25" (extract-result-text response))))
```

### Step 3: Add State Variables

```lisp
;; Create accumulator
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 5)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defvar *call-count* 0)")))))))))
  (assert (result-response-p response)))

;; Function that modifies state
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 6)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun tracked-square (x) (incf *call-count*) (square x))")))))))))
  (assert (result-response-p response)))
```

### Step 4: Verify State Accumulation

```lisp
;; First call
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 7)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(tracked-square 5))")))))))))
  (assert (result-response-p response))
  (assert (search "25" (extract-result-text response))))

;; Check count
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 8)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "*call-count*")))))))))
  (assert (search "1" (extract-result-text response))))

;; Second call
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 9)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(tracked-square 7))")))))))))
  (assert (result-response-p response))
  (assert (search "49" (extract-result-text response))))

;; Check count again
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 10)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "*call-count*")))))))))
  (assert (search "2" (extract-result-text response))))
```

### Step 5: Redefine Function

```lisp
;; Redefine to change behavior
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 11)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun square (x) (format nil \"squaring ~A\" x) (* x x))")))))))))
  (assert (result-response-p response)))

;; New definition is used
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 12)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(square 6))")))))))))
  (assert (result-response-p response))
  (assert (search "36" (extract-result-text response)))
  ;; Format output should be captured
  (assert (search "squaring" (extract-result-text response) :test #'char-equal)))
```

## Test: Package Workflow

### Step 1: Create Custom Package

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 20)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defpackage :math-utils (:use :cl) (:export :add-all))")))))))))
  (assert (result-response-p response)))

(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 21)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(in-package :math-utils)"))
                                             (:package . "CL-USER")))))))))
  (assert (result-response-p response)))
```

### Step 2: Define in Custom Package

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 22)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun add-all (&rest numbers) (reduce #'+ numbers))")))))))))
  (assert (result-response-p response)))
```

### Step 3: Verify Package Context Persists

```lisp
;; Still in math-utils
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 23)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(package-name *package*)")))))))))
  (assert (search "MATH-UTILS" (extract-result-text response))))

;; Can call function
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 24)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(add-all 1 2 3 4 5)")))))))))
  (assert (search "15" (extract-result-text response))))
```

### Step 4: Switch Back to CL-USER

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 25)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(in-package :cl-user)")))))))))
  (assert (result-response-p response)))

;; Use qualified name
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 26)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(math-utils:add-all 10 20 30)")))))))))
  (assert (search "60" (extract-result-text response))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Functions defined early must be available in later evaluations
- Redefinitions must take effect
- State in variables must accumulate across evaluations
- Package context must persist
- Interdependent definitions must work
