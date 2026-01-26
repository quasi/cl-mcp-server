---
type: property
name: request-response-guarantee
version: 1.0.0
feature: mcp-protocol
covers:
  - contracts/initialization
  - contracts/tools-list
  - contracts/tools-call
relates_to:
  - core/invariants#INV-001
---

# Request-Response Guarantee Property

## Statement

**For all** JSON-RPC requests (messages with an `id` field),
**if** the request is well-formed,
**then** the server MUST send exactly one response with matching `id`.

## Formal Expression

```
∀ request ∈ Requests :
  has_field(request, "id") ∧ is_valid_jsonrpc(request) ⟹
    ∃! response ∈ Responses :
      response.id = request.id ∧
      (has_field(response, "result") ⊕ has_field(response, "error"))
```

where `⊕` denotes exclusive or (exactly one of result or error, not both).

## Informal Explanation

Every request with an ID must receive exactly one response with the same ID. The response must have either a `result` (success) or `error` (failure), but never both and never neither.

This is true even when:
- The request method is unknown
- The request parameters are invalid
- An error occurs during processing
- The server encounters an internal error

Notifications (requests without `id`) do not receive responses.

## Rationale

Request-response pairing is fundamental to JSON-RPC. Claude Code relies on this to match responses to requests. If a request is silently dropped or receives multiple responses, Claude loses synchronization with the server.

## Counterexample Shape

If this property is violated, you might see:
- A request with `id: 42` receives no response
- A request receives multiple responses
- A response with a different `id` than the request
- A response with neither `result` nor `error`
- A response with both `result` and `error`

## Verification Approach

**Generator**: Generate random valid JSON-RPC requests with IDs

**Assertion**:
```lisp
(let ((responses (send-request-and-collect-responses request)))
  (and (= (length responses) 1)  ; exactly one response
       (equal (response-id (first responses))
              (request-id request))
       (xor (has-result (first responses))
            (has-error (first responses)))))
```

**Property Test**:
- Send 1000 random requests (valid and invalid methods)
- Verify each gets exactly one response
- Verify response ID matches request ID
- Verify response has result XOR error

**Edge Cases**:
- Unknown methods → error response
- Invalid parameters → error response
- Internal server error → error response
- All must still satisfy 1:1 request-response mapping

**Shrinking**: Find minimal request that violates the property
