---
type: verification
name: protocol-conformance-property-test
source: properties/protocol-conformance.md
level: property
tags:
  - property-based
  - conformance
---

# Property Test: Protocol Conformance

## Purpose

Verify all server messages conform to JSON-RPC 2.0 specification using property-based testing.

## Prerequisites

- Initialized server
- Property-based testing framework (e.g., cl-quickcheck or fiveam with randomized tests)

## Implementation

### Generator: Random Requests

```lisp
(defun generate-random-request ()
  "Generate random but structurally valid JSON-RPC requests"
  (let ((id (random 10000))
        (method (random-choice '("initialize" "tools/list" "tools/call"
                                "unknown/method"))))
    `((:jsonrpc . "2.0")
      (:id . ,id)
      (:method . ,method)
      (:params . ,(generate-random-params method)))))

(defun generate-random-params (method)
  (case method
    ("initialize" `((:protocolVersion . "2025-03-26")
                    (:capabilities . ())
                    (:clientInfo . ((:name . "test") (:version . "1.0")))))
    ("tools/list" nil)
    ("tools/call" `((:name . ,(random-choice '("evaluate-lisp" "unknown-tool")))
                    (:arguments . ((:code . "(+ 1 2)")))))
    (otherwise nil)))
```

### Property: All Responses Are Valid JSON-RPC

```lisp
(defun valid-jsonrpc-response-p (response)
  "Check if response conforms to JSON-RPC 2.0"
  (and
   ;; Has jsonrpc field with value "2.0"
   (assoc :jsonrpc response)
   (string= (cdr (assoc :jsonrpc response)) "2.0")

   ;; Has id field (matching request or null for errors)
   (assoc :id response)

   ;; Has exactly one of result or error
   (xor (assoc :result response)
        (assoc :error response))

   ;; If error, has code and message
   (or (not (assoc :error response))
       (let ((error (cdr (assoc :error response))))
         (and (assoc :code error)
              (numberp (cdr (assoc :code error)))
              (assoc :message error)
              (stringp (cdr (assoc :message error))))))))

(defun xor (a b)
  (and (or a b) (not (and a b))))
```

### Property Test

```lisp
(deftest protocol-conformance-property ()
  "All server responses conform to JSON-RPC 2.0"
  (let ((server (make-initialized-test-server)))
    (dotimes (i 1000)
      (let* ((request (generate-random-request))
             (response (send-request server request)))

        ;; Every response must be valid JSON-RPC
        (is (valid-jsonrpc-response-p response)
            "Response ~A does not conform to JSON-RPC 2.0" response)

        ;; ID must match (unless parse error)
        (unless (and (assoc :error response)
                     (= (cdr (assoc :code (cdr (assoc :error response)))) -32700))
          (is (equal (response-id response) (request-id request))
              "Response ID ~A doesn't match request ID ~A"
              (response-id response) (request-id request)))))))
```

### Malformed Input Test

```lisp
(defun generate-malformed-input ()
  "Generate invalid inputs to test error responses"
  (random-choice
   (list
    "{not json}"
    "{\"id\":1}"  ; missing jsonrpc and method
    "{\"jsonrpc\":\"2.0\"}"  ; missing method or result/error
    "{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"test\"}"  ; wrong version
    ""  ; empty
    "null"  ; just null
    )))

(deftest malformed-input-conformance ()
  "Even error responses to malformed input are valid JSON-RPC"
  (let ((server (make-initialized-test-server)))
    (dotimes (i 100)
      (let* ((input (generate-malformed-input))
             (response (send-raw-input server input)))
        ;; Error responses must still be valid JSON-RPC
        (is (valid-jsonrpc-response-p response)
            "Error response to ~A is not valid JSON-RPC: ~A"
            input response)

        ;; Must be error response
        (is (error-response-p response))))))
```

## Configuration

- Examples: 1000 for normal runs
- Examples: 10000 for CI/thorough testing
- Shrinking: Enabled (find minimal non-conforming case)

## Assertions

For every generated request:
1. Response must be valid JSON-RPC 2.0
2. Response ID must match request ID (except parse errors)
3. Response must have exactly one of `result` or `error`
4. If error, must have numeric `code` and string `message`

## Notes

- This tests the fundamental protocol layer
- Catches edge cases that manual tests might miss
- Shrinking helps find minimal reproducer for failures
