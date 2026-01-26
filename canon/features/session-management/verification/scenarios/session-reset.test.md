---
type: verification
name: session-reset-scenario-test
source: scenarios/session-reset.md
level: scenario
tags:
  - session
  - reset
---

# Scenario Test: Session Reset

## Purpose

Verify the complete session reset workflow clears all user-defined state.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test: Complete Reset Workflow

### Step 1: Accumulate State

```lisp
;; Define function
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 1)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun my-fn () 42)")))))))))
  (assert (result-response-p r1)))

;; Define variable
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 2)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defvar *my-var* 100)")))))))))
  (assert (result-response-p r2)))

;; Define macro
(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 3)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defmacro my-when (test &body body) `(when ,test ,@body))")))))))))
  (assert (result-response-p r3)))

;; Define class
(let ((r4 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 4)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defclass my-point () ((x :initarg :x) (y :initarg :y)))")))))))))
  (assert (result-response-p r4)))

;; Create custom package
(let ((r5 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 5)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defpackage :my-test-pkg (:use :cl))")))))))))
  (assert (result-response-p r5)))
```

### Step 2: Verify State Exists

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 6)
                    (:method . "tools/call")
                    (:params . ((:name . "list-definitions")
                               (:arguments . ())))))))
  (assert (result-response-p response))
  (let ((text (extract-result-text response)))
    (assert (search "MY-FN" text :test #'char-equal))
    (assert (search "*MY-VAR*" text :test #'char-equal))
    (assert (search "MY-WHEN" text :test #'char-equal))
    (assert (search "MY-POINT" text :test #'char-equal))))
```

### Step 3: Reset Session

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 7)
                    (:method . "tools/call")
                    (:params . ((:name . "reset-session")
                               (:arguments . ())))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "reset" text :test #'char-equal))
    (assert (search "CL-USER" text))))
```

### Step 4: Verify All State Cleared

#### Function Gone
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 8)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(my-fn)")))))))))
  (assert (is-error response))
  (assert (search "UNDEFINED-FUNCTION" (extract-result-text response) :test #'char-equal))
  (assert (search "MY-FN" (extract-result-text response) :test #'char-equal)))
```

#### Variable Gone
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 9)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "*my-var*")))))))))
  (assert (is-error response))
  (assert (search "UNBOUND-VARIABLE" (extract-result-text response) :test #'char-equal))
  (assert (search "*MY-VAR*" (extract-result-text response) :test #'char-equal)))
```

#### Macro Gone
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 10)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(my-when t 42)")))))))))
  (assert (is-error response))
  (assert (search "UNDEFINED-FUNCTION" (extract-result-text response) :test #'char-equal)))
```

#### Class Gone
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 11)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(make-instance 'my-point :x 1 :y 2)")))))))))
  (assert (is-error response))
  ;; Will fail because MY-POINT class doesn't exist
  (assert (search "error" (extract-result-text response) :test #'char-equal)))
```

#### Package Back to CL-USER
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 12)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(package-name *package*)")))))))))
  (assert (result-response-p response))
  (assert (search "CL-USER" (extract-result-text response))))
```

#### List Definitions Shows Empty
```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 13)
                    (:method . "tools/call")
                    (:params . ((:name . "list-definitions")
                               (:arguments . ())))))))
  (assert (result-response-p response))
  (let ((text (extract-result-text response)))
    ;; Should indicate no definitions
    (assert (or (search "no" text :test #'char-equal)
                (search "empty" text :test #'char-equal)
                (< (length text) 50)))))  ; Very short response
```

### Step 5: Verify Server Still Works

```lisp
;; Can define new things after reset
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 14)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(defun new-fn (x) (* x 3))")))))))))
  (assert (result-response-p response)))

;; New definition works
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 15)
                    (:method . "tools/call")
                    (:params . ((:name . "evaluate-lisp")
                               (:arguments . ((:code . "(new-fn 7)")))))))))
  (assert (result-response-p response))
  (assert (search "21" (extract-result-text response))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Reset must clear functions, variables, macros, classes, structures
- Reset must restore package to CL-USER
- Reset must not crash the server
- Server must be fully functional after reset
- User-created packages should be cleared (implementation-dependent)
