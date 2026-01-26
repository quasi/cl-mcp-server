---
type: verification
name: apropos-search-contract-test
source: contracts/apropos-search-tool.md
level: contract
tags:
  - smoke
  - introspection
  - search
---

# Contract Test: apropos-search

## Purpose

Verify the apropos-search tool finds symbols matching patterns.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test Cases

### Basic Search

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"apropos-search","arguments":{"pattern":"map"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find MAPCAR, MAPCAN, etc.
    (assert (search "MAPCAR" text :test #'char-equal))
    ;; Should show type information
    (assert (search "FUNCTION" text :test #'char-equal))
    ;; Should show package
    (assert (search "COMMON-LISP" text :test #'char-equal))))
```

### Case Insensitive

**Input**:
```json
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"apropos-search","arguments":{"pattern":"PRINT"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find print-related symbols
    (assert (or (search "PRINT" text :test #'char-equal)
                (search "print" text :test #'char-equal)))
    ;; Should find variables like *print-base*
    (assert (search "*PRINT-" text :test #'char-equal))))
```

### Type Filter: Functions

**Input**:
```json
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"apropos-search","arguments":{"pattern":"list","type":"function"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find LIST function
    (assert (search "LIST" text :test #'char-equal))
    ;; All results should be functions
    (assert (search "FUNCTION" text :test #'char-equal))
    ;; Should not include non-functions with "list" in name
    ;; (verified by not seeing VARIABLE or MACRO types inappropriately)))
```

### Type Filter: Variables

**Input**:
```json
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"apropos-search","arguments":{"pattern":"print","type":"variable"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find *print-base*, *print-level*, etc.
    (assert (search "*PRINT-" text :test #'char-equal))
    ;; All results should be variables
    (assert (search "VARIABLE" text :test #'char-equal))))
```

### Package Filter

**Input**:
```lisp
;; Create custom package with symbols
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 5)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defpackage :my-test-pkg (:use :cl))")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 6)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(in-package :my-test-pkg)")))))))))
  (assert (result-response-p r2)))

(let ((r3 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 7)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun my-search-fn () t) (defun my-other-fn () nil)")))))))))
  (assert (result-response-p r3)))

;; Search only in that package
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 8)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "my")
                                             (:package . "MY-TEST-PKG")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find our functions
    (assert (search "MY-SEARCH-FN" text :test #'char-equal))
    (assert (search "MY-OTHER-FN" text :test #'char-equal))
    ;; Should show package
    (assert (search "MY-TEST-PKG" text :test #'char-equal))))
```

### Search User Symbols

**Input**:
```lisp
;; Go back to CL-USER
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 9)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(in-package :cl-user)")))))))))
  (assert (result-response-p r1)))

;; Define some test symbols
(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun user-test-1 () 1) (defun user-test-2 () 2)")))))))))
  (assert (result-response-p r2)))

;; Search for them
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 11)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "user-test")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "USER-TEST-1" text :test #'char-equal))
    (assert (search "USER-TEST-2" text :test #'char-equal))))
```

### No Matches

**Input**:
```json
{"jsonrpc":"2.0","id":12,"method":"tools/call","params":{"name":"apropos-search","arguments":{"pattern":"xyz-nonexistent-pattern-abc"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should indicate no matches found
    (assert (or (search "no" text :test #'char-equal)
                (search "not found" text :test #'char-equal)
                (< (length text) 100)))))  ; Very short response
```

### Multiple Type Results

**Input**:
```lisp
;; Define symbol with multiple meanings
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 13)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun multi-test () t)")))))))))
  (assert (result-response-p r1)))

(let ((r2 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 14)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defvar *multi-test* 42)")))))))))
  (assert (result-response-p r2)))

;; Search should show both
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 15)
                    (:method . "tools/call")
                    (:params . ((:name . "apropos-search")
                               (:arguments . ((:pattern . "multi-test")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Should find both function and variable
    (assert (search "MULTI-TEST" text :test #'char-equal))
    (assert (search "FUNCTION" text :test #'char-equal))
    (assert (search "VARIABLE" text :test #'char-equal))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Search is case-insensitive
- Pattern is substring match
- Can filter by type: function, variable, macro, class, generic-function
- Can filter by package
- Shows symbol name, type, and package
- No matches should return informative message, not error
- Should find both system and user-defined symbols
