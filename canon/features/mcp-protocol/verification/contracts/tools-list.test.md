---
type: verification
name: tools-list-contract-test
source: contracts/tools-list.md
level: contract
tags:
  - smoke
  - tools
---

# Contract Test: Tools List

## Purpose

Verify the tools/list contract returns available tools with correct schemas.

## Prerequisites

- Initialized server

## Setup

```lisp
(defvar *server* (make-initialized-test-server))
```

## Test Cases

### Successful Tools List

**Input**:
```json
{"jsonrpc":"2.0","id":1,"method":"tools/list"}
```

**Expected**:
```lisp
(let ((response (send-request *server* input)))
  ;; Success response
  (assert (result-response-p response))

  ;; Has tools array
  (let ((tools (result-field response :tools)))
    (assert (listp tools))
    (assert (> (length tools) 0))

    ;; Check evaluate-lisp tool is present
    (let ((eval-tool (find "evaluate-lisp" tools
                          :key (lambda (t) (cdr (assoc :name t)))
                          :test #'string=)))
      (assert eval-tool)

      ;; Has required fields
      (assert (assoc :name eval-tool))
      (assert (assoc :description eval-tool))
      (assert (assoc :inputSchema eval-tool))

      ;; Input schema is valid JSON Schema
      (let ((schema (cdr (assoc :inputSchema eval-tool))))
        (assert (string= (cdr (assoc :type schema)) "object"))
        (assert (assoc :required schema))
        (assert (member "code" (cdr (assoc :required schema)) :test #'string=))))))
```

### Before Initialization

**Input**: Call tools/list before initialize

**Expected**:
```lisp
(let ((fresh-server (cl-mcp-server:start-test-server)))
  (let ((response (send-request fresh-server input)))
    (assert (error-response-p response))
    (assert (search "not initialized" (error-message response) :test #'char-equal))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- Must return all available tools
- Each tool must have name, description, and inputSchema
- Input schema must be valid JSON Schema
- Should fail if called before initialization
