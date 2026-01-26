---
type: verification
name: initialization-handshake-scenario-test
source: scenarios/initialization-handshake.md
level: scenario
tags:
  - smoke
  - integration
---

# Scenario Test: Initialization Handshake

## Purpose

Verify the complete initialization handshake flow from fresh server to ready state.

## Prerequisites

- Fresh server process
- Stdio transport

## Setup

```lisp
(defvar *server* (cl-mcp-server:start-test-server))
(defvar *transcript* '())  ; Record all messages

(defun send-and-record (request)
  (let ((response (send-request *server* request)))
    (push (cons :request request) *transcript*)
    (push (cons :response response) *transcript*)
    response))
```

## Execution

### Step 1: Client sends initialize request

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 1)
                   (:method . "initialize")
                   (:params . ((:protocolVersion . "2025-03-26")
                              (:capabilities . ())
                              (:clientInfo . ((:name . "test-client")
                                            (:version . "1.0.0")))))))))
  ;; Verify response
  (assert (result-response-p response))
  (assert (= (response-id response) 1))
  (assert (string= (result-field response :protocolVersion) "2025-03-26"))
  (assert (result-field response :serverInfo)))
```

### Step 2: Client sends initialized notification

```lisp
(let ((response (send-notification *server*
                                   '((:jsonrpc . "2.0")
                                     (:method . "notifications/initialized")))))
  ;; Notifications don't receive responses
  (assert (null response)))
```

### Step 3: Client discovers tools

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 2)
                   (:method . "tools/list")))))
  ;; Verify tools list response
  (assert (result-response-p response))
  (assert (= (response-id response) 2))

  (let ((tools (result-field response :tools)))
    (assert (> (length tools) 0))
    ;; Should have evaluate-lisp tool
    (assert (find "evaluate-lisp" tools
                 :key (lambda (t) (cdr (assoc :name t)))
                 :test #'string=))))
```

## Verification

### Request-Response Pairing

```lisp
;; Check all requests got responses
(let ((requests (remove-if-not (lambda (m) (eq (car m) :request)) *transcript*))
      (responses (remove-if-not (lambda (m) (eq (car m) :response)) *transcript*)))
  (assert (= (length requests) (length responses))))
```

### Protocol Conformance

```lisp
;; All messages are valid JSON-RPC
(dolist (msg *transcript*)
  (let ((content (cdr msg)))
    (assert (assoc :jsonrpc content))
    (assert (string= (cdr (assoc :jsonrpc content)) "2.0"))))
```

### State Transition

```lisp
;; Server should now be initialized
(assert (server-initialized-p *server*))

;; Can call tools now
(let ((response (send-request *server* '((:jsonrpc . "2.0")
                                         (:id . 3)
                                         (:method . "tools/list")))))
  (assert (result-response-p response)))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *server*)
```

## Notes

- This tests the full happy path from startup to ready
- Verifies all three steps of the handshake
- Confirms server state transitions correctly
