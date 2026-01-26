---
type: property
name: error-response-stability
version: 1.0.0
feature: mcp-protocol
covers:
  - contracts/initialization
  - contracts/tools-list
  - contracts/tools-call
  - contracts/transport
relates_to:
  - core/invariants#INV-002
---

# Error Response Stability Property

## Statement

**For all** malformed or invalid requests,
**the server** MUST return a well-formed JSON-RPC error response
AND **the server** MUST remain operational for subsequent requests.

## Formal Expression

```
∀ request ∈ InvalidRequests :
  let response = handle(request) in
    is_valid_jsonrpc_error(response) ∧
    server_is_operational_after(request)

where InvalidRequests includes:
  - Malformed JSON
  - Missing required fields
  - Unknown methods
  - Invalid parameters
  - Type mismatches
```

## Informal Explanation

When the server receives bad input, it must:

1. **Respond gracefully**: Return a proper JSON-RPC error (not crash)
2. **Stay alive**: Continue accepting and processing new requests
3. **Categorize correctly**: Use appropriate error codes:
   - `-32700`: Parse error (malformed JSON)
   - `-32600`: Invalid Request (missing fields)
   - `-32601`: Method not found
   - `-32602`: Invalid params

No matter how broken the input is, the server must survive and explain what went wrong.

## Rationale

User code may contain bugs. Network issues may corrupt messages. The server must be resilient to all forms of invalid input. Crashing on bad input would lose session state and interrupt Claude's workflow.

## Counterexample Shape

If this property is violated, you might see:
- Server process terminates on malformed JSON
- Server hangs on invalid request
- Server sends malformed response to invalid request
- Server stops accepting requests after seeing bad input
- Server crashes with unhandled error

## Verification Approach

**Fuzz Testing**:

Generate malformed inputs:
- Random byte strings (invalid JSON)
- Valid JSON with missing fields
- JSON with wrong field types
- Unknown method names
- Invalid parameter values
- Extremely large inputs
- Special characters and unicode

**Assertion**:
```lisp
(defun test-error-stability (invalid-request)
  (let ((server (make-initialized-server)))
    ;; Send invalid request
    (let ((response (send-request server invalid-request)))
      ;; Should get error response, not crash
      (assert (error-response-p response))
      (assert (valid-jsonrpc-p response)))

    ;; Server should still work
    (let ((response2 (send-request server '(:method "tools/list"))))
      (assert (result-response-p response2)))))
```

**Error Code Verification**:
```lisp
(assert (= (error-code response) -32700))  ; Parse error
(assert (= (error-code response) -32600))  ; Invalid Request
(assert (= (error-code response) -32601))  ; Method not found
(assert (= (error-code response) -32602))  ; Invalid params
```

**Stress Test**:
- Send 10,000 invalid requests in sequence
- Verify server responds to all with errors
- Verify server remains operational throughout
- Send valid request at end to confirm server still works

**Shrinking**: Find minimal invalid input that causes server failure
