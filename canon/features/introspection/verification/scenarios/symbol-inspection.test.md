---
type: verification
name: symbol-inspection-scenario-test
source: scenarios/symbol-inspection.md
level: scenario
tags:
  - introspection
  - workflow
---

# Scenario Test: Symbol Inspection

## Purpose

Verify the complete workflow for discovering and inspecting symbols.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test: Discovery and Inspection Workflow

### Step 1: Discover Symbols with apropos

```lisp
;; User wants to find list-related functions
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 1)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "list")
                                             (:type . "function")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find multiple list functions
    (assert (search "LIST" text :test #'char-equal))
    (assert (search "FUNCTION" text :test #'char-equal))))
```

### Step 2: Inspect Specific Symbol

```lisp
;; Now get detailed info on LIST function
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 2)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "list")
                                             (:package . "CL")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should have comprehensive info
    (assert (search "FUNCTION" text :test #'char-equal))
    (assert (search "Arglist:" text))
    (assert (search "REST" text))  ; LIST takes &rest
    (assert (search "Documentation:" text))))
```

### Step 3: Inspect Related Symbol

```lisp
;; Check out MAPCAR which uses lists
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 3)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "mapcar")
                                             (:package . "CL")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "FUNCTION" text :test #'char-equal))
    (assert (search "Arglist:" text))
    (assert (search "FUNCTION" text))
    (assert (search "LIST" text))))
```

## Test: User-Defined Symbol Workflow

### Step 1: Define Custom Functions

```lisp
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun my-utility-1 (x) \"First utility\" (* x 2))")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 11)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun my-utility-2 (x y) \"Second utility\" (+ x y))")))))))))
  (assert (result-response-p r2)))

(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 12)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defvar *my-config* :production \"Config setting\")")))))))))
  (assert (result-response-p r3)))
```

### Step 2: Search Own Definitions

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 13)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "my-utility")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "MY-UTILITY-1" text :test #'char-equal))
    (assert (search "MY-UTILITY-2" text :test #'char-equal))
    (assert (search "FUNCTION" text :test #'char-equal))))
```

### Step 3: Inspect User Definition

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 14)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "my-utility-1")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "FUNCTION" text :test #'char-equal))
    (assert (search "Arglist:" text))
    (assert (search "X" text))
    (assert (search "First utility" text))  ; docstring
    (assert (search "Documentation:" text))))
```

### Step 4: Search for Variables

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 15)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "my-config")
                                             (:type . "variable")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "*MY-CONFIG*" text :test #'char-equal))
    (assert (search "VARIABLE" text :test #'char-equal))))
```

### Step 5: Inspect Variable

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 16)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "*my-config*")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "VARIABLE" text :test #'char-equal))
    (assert (search "Value:" text))
    (assert (search "PRODUCTION" text :test #'char-equal))
    (assert (search "Config setting" text))))  ; docstring
```

## Test: Package-Scoped Inspection

### Step 1: Create Package with Symbols

```lisp
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 20)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defpackage :inspection-test (:use :cl) (:export :public-fn))")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 21)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(in-package :inspection-test)")))))))))
  (assert (result-response-p r2)))

(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 22)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun public-fn () t) (defun private-fn () nil)")))))))))
  (assert (result-response-p r3)))
```

### Step 2: Search in Package

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 23)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "fn")
                                             (:package . "INSPECTION-TEST")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "PUBLIC-FN" text :test #'char-equal))
    (assert (search "PRIVATE-FN" text :test #'char-equal))))
```

### Step 3: Inspect Package Symbol

```lisp
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 24)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "public-fn")
                                             (:package . "INSPECTION-TEST")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "FUNCTION" text :test #'char-equal))
    (assert (search "INSPECTION-TEST" text :test #'char-equal))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Workflow: search → discover → inspect
- Works for both system and user-defined symbols
- Package filtering enables focused searches
- Inspection provides full details needed for usage
- Supports iterative exploration (one symbol leads to related symbols)
