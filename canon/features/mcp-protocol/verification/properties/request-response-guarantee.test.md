---
type: verification
name: request-response-guarantee-property-test
source: properties/request-response-guarantee.md
level: property
tags:
  - property-based
  - critical
---

# Property Test: Request-Response Guarantee

## Purpose

Verify every request receives exactly one response with matching ID using property-based testing.

## Prerequisites

- Initialized server
- Property-based testing framework

## Implementation

### Generator: Request Sequences

```lisp
(defun generate-request-sequence (n)
  "Generate N requests with unique IDs"
  (loop for i from 1 to n
        collect `((:jsonrpc . "2.0")
                  (:id . ,i)
                  (:method . ,(random-choice '("tools/list" "tools/call")))
                  (:params . ,(generate-valid-params)))))

(defun generate-valid-params ()
  (random-choice
   (list nil  ; for tools/list
         `((:name . "evaluate-lisp")
           (:arguments . ((:code . ,(generate-random-lisp-code))))))))

(defun generate-random-lisp-code ()
  (random-choice
   '("(+ 1 2)" "(list 1 2 3)" "(format t \"hello\")" "(defun foo () 42)")))
```

### Property: 1:1 Request-Response Mapping

```lisp
(deftest one-to-one-request-response ()
  "Every request gets exactly one response with matching ID"
  (let ((server (make-initialized-test-server)))
    (dotimes (trial 100)
      (let* ((num-requests (+ 1 (random 20)))  ; 1-20 requests
             (requests (generate-request-sequence num-requests))
             (responses (mapcar (lambda (req) (send-request server req))
                               requests)))

        ;; Same number of responses as requests
        (is (= (length responses) (length requests))
            "Expected ~A responses, got ~A" (length requests) (length responses))

        ;; Each response ID matches its request ID
        (loop for request in requests
              for response in responses
              do (is (equal (response-id response) (request-id request))
                     "Response ID ~A doesn't match request ID ~A"
                     (response-id response) (request-id request)))

        ;; All IDs are unique (no duplicates)
        (let ((response-ids (mapcar #'response-id responses)))
          (is (= (length response-ids) (length (remove-duplicates response-ids)))
              "Duplicate response IDs detected"))))))
```

### Property: No Dropped Requests

```lisp
(deftest no-dropped-requests ()
  "All requests receive responses, even under load"
  (let ((server (make-initialized-test-server))
        (num-requests 1000)
        (request-ids (loop for i from 1 to 1000 collect i))
        (response-ids '()))

    ;; Send many requests
    (dolist (id request-ids)
      (let ((response (send-request server
                                    `((:jsonrpc . "2.0")
                                      (:id . ,id)
                                      (:method . "tools/list")))))
        (push (response-id response) response-ids)))

    ;; All requests got responses
    (is (= (length response-ids) num-requests)
        "Some requests were dropped")

    ;; All response IDs match request IDs
    (is (null (set-difference request-ids response-ids))
        "Missing response IDs: ~A" (set-difference request-ids response-ids))

    ;; No duplicate responses
    (is (= (length response-ids) (length (remove-duplicates response-ids)))
        "Duplicate responses detected")))
```

### Property: Error Requests Also Get Responses

```lisp
(deftest error-requests-get-responses ()
  "Invalid requests still get error responses (not dropped)"
  (let ((server (make-initialized-test-server)))
    (dotimes (i 100)
      (let* ((invalid-request `((:jsonrpc . "2.0")
                                (:id . ,i)
                                (:method . "nonexistent/method")))
             (response (send-request server invalid-request)))

        ;; Gets a response
        (is (not (null response)) "No response for invalid request ~A" i)

        ;; Response has matching ID
        (is (= (response-id response) i)
            "Response ID doesn't match for error case")

        ;; Is an error response
        (is (error-response-p response)
            "Expected error response for invalid method")))))
```

### Property: Concurrent Requests

```lisp
(deftest concurrent-request-response ()
  "All requests get responses even when sent concurrently"
  (let ((server (make-initialized-test-server))
        (num-threads 10)
        (requests-per-thread 50)
        (all-responses (make-hash-table :test 'equal)))

    ;; Spawn threads to send requests concurrently
    (let ((threads
           (loop for thread-id below num-threads
                 collect
                 (bt:make-thread
                  (lambda ()
                    (loop for i from 1 to requests-per-thread
                          do (let* ((id (format nil "~A-~A" thread-id i))
                                   (request `((:jsonrpc . "2.0")
                                            (:id . ,id)
                                            (:method . "tools/list")))
                                   (response (send-request server request)))
                              (setf (gethash id all-responses) response))))
                  :name (format nil "Requester-~A" thread-id)))))

      ;; Wait for all threads
      (mapc #'bt:join-thread threads))

    ;; Verify all requests got responses
    (is (= (hash-table-count all-responses)
           (* num-threads requests-per-thread))
        "Some concurrent requests didn't get responses")

    ;; Verify all IDs match
    (maphash (lambda (request-id response)
               (is (equal (response-id response) request-id)
                   "Concurrent request ~A got wrong response ID" request-id))
             all-responses)))
```

## Configuration

- Examples: 100 trials with 1-20 requests each
- Stress test: 1000 sequential requests
- Concurrency test: 10 threads × 50 requests
- Shrinking: Enabled

## Assertions

1. Every request produces exactly one response
2. Response ID always matches request ID
3. No requests are dropped (even invalid ones)
4. No duplicate responses
5. Holds true under concurrent load

## Notes

- This is a critical property - failure means Claude loses sync
- Test both valid and invalid requests
- Test under load and concurrency
- Shrinking will find minimal failing sequence
