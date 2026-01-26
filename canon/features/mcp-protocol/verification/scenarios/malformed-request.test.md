---
type: verification
name: malformed-request-scenario-test
source: scenarios/malformed-request.md
level: scenario
tags:
  - error-handling
  - robustness
---

# Scenario Test: Malformed Request Handling

## Purpose

Verify the server handles all forms of invalid input gracefully without crashing.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))

(defun send-raw-input (server raw-string)
  "Send raw string to server, bypassing normal JSON encoding"
  (write-string raw-string (server-stdin server))
  (force-output (server-stdin server))
  (read-response (server-stdout server)))
```

## Test Cases

### Case 1: Invalid JSON

**Input**: `{not valid json}`

**Expected**:
```lisp
(let ((response (send-raw-input *server* "{not valid json}\n")))
  (assert (error-response-p response))
  (assert (= (error-code response) -32700))  ; Parse error
  (assert (null (response-id response))))  ; id is null for parse errors
```

### Case 2: Missing jsonrpc field

**Input**:
```json
{"id":1,"method":"tools/list"}
```

**Expected**:
```lisp
(let ((response (send-raw-input *server* "{\"id\":1,\"method\":\"tools/list\"}\n")))
  (assert (error-response-p response))
  (assert (= (error-code response) -32600))  ; Invalid Request
  (assert (= (response-id response) 1))
  (assert (search "jsonrpc" (error-message response) :test #'char-equal)))
```

### Case 3: Unknown method

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"unknown/method"}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (= (error-code response) -32601))  ; Method not found
  (assert (= (response-id response) 1))
  (assert (search "unknown/method" (error-message response) :test #'char-equal)))
```

### Case 4: Invalid tool name

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"nonexistent-tool","arguments":{}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602))  ; Invalid params
  (assert (search "nonexistent-tool" (error-message response) :test #'char-equal)))
```

### Case 5: Missing required parameter

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602))  ; Invalid params
  (assert (search "code" (error-message response) :test #'char-equal)))
```

## Verification

### Server Remains Operational

After each error, verify server still works:

```lisp
(defun verify-server-operational ()
  (let ((response (send-request *server* '((:jsonrpc . "2.0")
                                           (:id . 999)
                                           (:method . "tools/list")))))
    (assert (result-response-p response))
    (assert (= (response-id response) 999))))

;; After each test case
(verify-server-operational)
```

### All Errors Are Valid JSON-RPC

```lisp
(defun verify-error-response (response expected-code)
  ;; Is valid JSON-RPC error
  (assert (assoc :jsonrpc response))
  (assert (string= (cdr (assoc :jsonrpc response)) "2.0"))
  (assert (assoc :error response))
  (assert (not (assoc :result response)))

  ;; Error has code and message
  (let ((error (cdr (assoc :error response))))
    (assert (assoc :code error))
    (assert (= (cdr (assoc :code error)) expected-code))
    (assert (assoc :message error))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Server must handle all error cases without crashing
- Each error must have appropriate JSON-RPC error code
- Server must remain operational after errors
- Error messages should be informative
