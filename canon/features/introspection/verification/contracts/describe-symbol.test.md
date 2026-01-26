---
type: verification
name: describe-symbol-contract-test
source: contracts/describe-symbol-tool.md
level: contract
tags:
  - smoke
  - introspection
---

# Contract Test: describe-symbol

## Purpose

Verify the describe-symbol tool provides comprehensive symbol information.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test Cases

### Standard Function

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"mapcar","package":"CL"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "FUNCTION" text :test #'char-equal))
    ;; Must include arglist
    (assert (search "Arglist:" text))
    (assert (search "FUNCTION" text))
    (assert (search "LIST" text))
    ;; Must include documentation
    (assert (search "Documentation:" text))
    ;; Symbol name should appear
    (assert (search "MAPCAR" text :test #'char-equal))))
```

### Special Variable

**Input**:
```json
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"*print-base*","package":"CL"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "VARIABLE" text :test #'char-equal))
    ;; Must include current value
    (assert (search "Value:" text))
    (assert (search "10" text))  ; default value
    ;; Should have documentation
    (assert (search "Documentation:" text))))
```

### Macro

**Input**:
```json
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"defun","package":"CL"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "MACRO" text :test #'char-equal))
    ;; Must include arglist
    (assert (search "Arglist:" text))
    ;; Should have documentation
    (assert (search "Documentation:" text))))
```

### Class

**Input**:
```json
{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"standard-class","package":"CL"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "CLASS" text :test #'char-equal))))
```

### Generic Function

**Input**:
```json
{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"print-object","package":"CL"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "GENERIC-FUNCTION" text :test #'char-equal))
    ;; Must include arglist
    (assert (search "Arglist:" text))
    (assert (search "OBJECT" text))
    (assert (search "STREAM" text))))
```

### User-Defined Symbol

**Input**:
```lisp
;; Define a function first
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 6)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defun my-test-fn (x y) \"Adds two numbers\" (+ x y))")))))))))
  (assert (result-response-p r1)))

;; Now describe it
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 7)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "my-test-fn")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    ;; Must include type
    (assert (search "FUNCTION" text :test #'char-equal))
    ;; Must include arglist
    (assert (search "Arglist:" text))
    (assert (search "X" text))
    (assert (search "Y" text))
    ;; Should include docstring
    (assert (search "Adds two numbers" text))))
```

### Symbol Not Found

**Input**:
```json
{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"nonexistent-symbol-xyz"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  ;; Should not be an error, just informative
  (let ((text (extract-result-text response)))
    (assert (search "not found" text :test #'char-equal))
    (assert (search "NONEXISTENT-SYMBOL-XYZ" text :test #'char-equal))))
```

### Package Not Found

**Input**:
```json
{"jsonrpc":"2.0","id":9,"method":"tools/call","params":{"name":"describe-symbol","arguments":{"name":"foo","package":"NONEXISTENT-PKG"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (result-response-p response))
  (let ((text (extract-result-text response)))
    (assert (search "Package" text :test #'char-equal))
    (assert (search "not found" text :test #'char-equal))
    (assert (search "NONEXISTENT-PKG" text :test #'char-equal))))
```

### Default Package Behavior

**Input**:
```lisp
;; Define in CL-USER (default)
(let ((r1 (send-request *server*
            '((:jsonrpc . "2.0")
              (:id . 10)
              (:method . "tools/call")
              (:params . ((:name . "evaluate-lisp")
                         (:arguments . ((:code . "(defvar *test-default* 123)")))))))))
  (assert (result-response-p r1)))

;; Describe without specifying package (should default to CL-USER)
(let ((response (send-request *server*
                  '((:jsonrpc . "2.0")
                    (:id . 11)
                    (:method . "tools/call")
                    (:params . ((:name . "describe-symbol")
                               (:arguments . ((:name . "*test-default*")))))))))
  (assert (result-response-p response))
  (assert (not (is-error response)))
  (let ((text (extract-result-text response)))
    (assert (search "VARIABLE" text :test #'char-equal))
    (assert (search "123" text))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Must return type information for all symbol types
- Functions and macros must include arglists
- Variables must include current value (with print limits)
- Documentation should be included when available
- Source location information is optional (SBCL-specific)
- Symbol-not-found should be informative, not an error
- Default package is CL-USER when not specified
