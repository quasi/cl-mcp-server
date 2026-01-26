---
type: verification
name: basic-evaluation-scenario-test
source: scenarios/basic-evaluation.md
level: scenario
tags:
  - smoke
  - integration
---

# Scenario Test: Basic Code Evaluation

## Purpose

Verify the complete workflow of evaluating simple Lisp expressions through the MCP interface.

## Prerequisites

- Fresh server instance initialized with MCP handshake
- Default session state (CL-USER package)

## Setup

```lisp
(defvar *test-server* (cl-mcp-server:start-test-server))
(defvar *transcript* '())  ; Record all messages

(defun send-and-record (request)
  (let ((response (send-request *test-server* request)))
    (push (cons :request request) *transcript*)
    (push (cons :response response) *transcript*)
    response))
```

## Execution

### Step 1: Evaluate Arithmetic Expression

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 1)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(+ 1 2 3)")))))))))
  ;; Verify success
  (assert (result-response-p response))
  (assert (= (response-id response) 1))

  ;; Check result
  (let ((text (result-content-text response)))
    (assert (string= "=> 6" (string-trim '(#\Space #\Newline) text)))))
```

### Step 2: Evaluate String Operation

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 2)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(concatenate 'string \"Hello, \" \"World!\")")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    (assert (search "Hello, World!" text))))
```

### Step 3: Evaluate List Operation

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 3)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(mapcar #'1+ '(1 2 3 4 5))")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    (assert (search "(2 3 4 5 6)" text))))
```

### Step 4: Evaluate Expression with Multiple Values

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 4)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(floor 17 5)")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    ;; Should see both values
    (assert (search "=> 3" text))
    (assert (search "=> 2" text))))
```

### Step 5: Evaluate Expression Returning NIL

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 5)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(member 'x '(a b c))")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    (assert (search "NIL" text))))
```

### Step 6: Evaluate Multiple Forms (Only Last Result Returned)

```lisp
(let ((response (send-and-record
                 '((:jsonrpc . "2.0")
                   (:id . 6)
                   (:method . "tools/call")
                   (:params . ((:name . "evaluate-lisp")
                              (:arguments . ((:code . "(setf x 10) (setf y 20) (+ x y)")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    ;; Only last form's result
    (assert (search "=> 30" text))
    ;; Should NOT see intermediate results
    (let ((result-lines (count-occurrences "=>" text)))
      (assert (= result-lines 1)))))
```

## Verification

### All Requests Got Responses

```lisp
(let ((requests (remove-if-not (lambda (m) (eq (car m) :request)) *transcript*))
      (responses (remove-if-not (lambda (m) (eq (car m) :response)) *transcript*)))
  (assert (= (length requests) (length responses)))
  (assert (= 6 (length requests))))
```

### All Responses Are Valid JSON-RPC

```lisp
(dolist (msg *transcript*)
  (when (eq (car msg) :response)
    (let ((content (cdr msg)))
      (assert (assoc :jsonrpc content))
      (assert (string= (cdr (assoc :jsonrpc content)) "2.0"))
      (assert (assoc :id content)))))
```

### Response IDs Match Request IDs

```lisp
(let ((pairs (loop for i from 0 below (length *transcript*) by 2
                  collect (cons (nth i *transcript*) (nth (1+ i) *transcript*)))))
  (dolist (pair pairs)
    (let ((request (cdr (car pair)))
          (response (cdr (cdr pair))))
      (assert (= (cdr (assoc :id request))
                (cdr (assoc :id response)))))))
```

### No Errors in Basic Operations

```lisp
(dolist (msg *transcript*)
  (when (eq (car msg) :response)
    (let ((response (cdr msg)))
      (assert (not (assoc :error response))))))
```

### Session State Modified

```lisp
;; Variables x and y from Step 6 should persist
(let ((response (send-request *test-server*
                              '((:jsonrpc . "2.0")
                                (:id . 7)
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . "(list x y)")))))))))
  (assert (result-response-p response))
  (let ((text (result-content-text response)))
    (assert (search "(10 20)" text))))
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *test-server*)
```

## Notes

- This tests the happy path for basic evaluation
- All operations complete successfully
- State persists across evaluations (Step 6 sets variables)
- Multiple values are properly formatted (Step 4)
- NIL results are explicitly shown (Step 5)
- Only last form's result is returned (Step 6)
