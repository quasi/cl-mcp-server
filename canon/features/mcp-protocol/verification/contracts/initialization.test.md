---
type: verification
name: initialization-contract-test
source: contracts/initialization.md
level: contract
tags:
  - smoke
  - initialization
---

# Contract Test: Initialization

## Purpose

Verify the MCP initialization handshake contract is correctly implemented.

## Prerequisites

- Fresh server instance (uninitialized state)
- Stdio transport available

## Setup

```lisp
(defvar *server* (cl-mcp-server:start-test-server))
```

## Test Cases

### Valid Initialization

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  ;; Status: success
  (assert (result-response-p response))

  ;; ID matches
  (assert (= (response-id response) 1))

  ;; Protocol version matches
  (assert (string= (result-field response :protocolVersion) "2025-03-26"))

  ;; Has server info
  (assert (assoc :serverInfo (result response)))
  (assert (string= (result-field response :serverInfo :name) "cl-mcp-server"))

  ;; Has capabilities
  (assert (assoc :capabilities (result response))))
```

### Missing Protocol Version

**Input**:
```json
{"jsonrpc":"2.0","id":2,"method":"initialize","params":{"capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602)))  ; Invalid params
```

### Missing Client Info

**Input**:
```json
{"jsonrpc":"2.0","id":3,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (= (error-code response) -32602)))  ; Invalid params
```

### Incompatible Protocol Version

**Input**:
```json
{"jsonrpc":"2.0","id":4,"method":"initialize","params":{"protocolVersion":"9999-99-99","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  (assert (error-response-p response))
  (assert (search "protocol version" (error-message response) :test #'char-equal)))
```

### Double Initialization

**Input**: Send initialize twice

**Expected**:
```lisp
;; First succeeds
(assert (result-response-p (send-request *server* init-request)))

;; Second fails
(let ((response (send-request *server* init-request)))
  (assert (error-response-p response))
  (assert (search "already initialized" (error-message response) :test #'char-equal)))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Server must validate all required fields
- Server must check protocol version compatibility
- Server must prevent re-initialization
