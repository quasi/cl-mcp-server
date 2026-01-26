---
type: property
name: initialization-state-machine
version: 1.0.0
feature: mcp-protocol
covers:
  - contracts/initialization
---

# Initialization State Machine Property

## Statement

**For all** MCP requests,
**if** the server is in `uninitialized` state,
**then** only the `initialize` method is accepted; all other methods produce errors.

**For all** MCP requests,
**if** the server is in `initialized` state,
**then** `initialize` method produces an error; other methods are accepted.

## Formal Expression

```
∀ request ∈ Requests :
  server.state = uninitialized ⟹
    (request.method ≠ "initialize" ⟹
      response_is_error(handle(request)))

∀ request ∈ Requests :
  server.state = initialized ⟹
    (request.method = "initialize" ⟹
      response_is_error(handle(request)))
```

State transition:
```
uninitialized --[initialize]--> initialized
```

This transition is **irreversible** (no going back to uninitialized without restart).

## Informal Explanation

The MCP server has two states:

1. **Uninitialized**: After startup, before the initialize handshake
   - Only `initialize` requests are valid
   - All other requests produce "Server not initialized" errors

2. **Initialized**: After successful initialize handshake
   - `initialize` requests produce "Already initialized" errors
   - All other methods (`tools/list`, `tools/call`, etc.) are accepted

The state machine ensures clients cannot use the server without completing initialization, and cannot re-initialize a running session.

## Rationale

The initialization handshake establishes protocol version compatibility and capability negotiation. Processing requests before initialization would bypass version checking and capability declaration. Re-initialization could reset session state unexpectedly.

## Counterexample Shape

If this property is violated, you might see:
- `tools/list` succeeding before `initialize` is called
- `tools/call` working in uninitialized state
- `initialize` succeeding twice
- Server resetting state when re-initialized

## Verification Approach

**State Machine Test**:

```lisp
(defun test-initialization-state-machine ()
  ;; Start: server is uninitialized
  (let ((server (make-fresh-server)))

    ;; Pre-initialization: tools/list should fail
    (assert (error-response-p
             (send-request server '(:method "tools/list"))))

    ;; Initialize: should succeed
    (let ((response (send-request server
                                  '(:method "initialize"
                                    :params (...)))))
      (assert (result-response-p response)))

    ;; Post-initialization: tools/list should succeed
    (assert (result-response-p
             (send-request server '(:method "tools/list"))))

    ;; Re-initialize: should fail
    (assert (error-response-p
             (send-request server
                           '(:method "initialize"
                             :params (...)))))))
```

**State Coverage**:
- Test all methods in uninitialized state (all except initialize should fail)
- Test all methods in initialized state (initialize should fail, others succeed)

**Transition Test**:
- Verify state changes from uninitialized → initialized
- Verify no transition back to uninitialized

**Shrinking**: Not applicable (state machine is deterministic)
