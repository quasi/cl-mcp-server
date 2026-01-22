# JSON-RPC Core Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement JSON-RPC 2.0 message parsing, encoding, and error handling as the foundation for MCP communication.

**Architecture:** Struct-based message representation with yason for JSON. Condition system for protocol errors. Pure functions for parsing/encoding.

**Tech Stack:** SBCL, yason, FiveAM

**Dependencies:** Plan 00 (Project Bootstrap)

**Blocks:** Plans 02-05 (all features need JSON-RPC)

**Spec Reference:** `canon/core/contracts/shared-types.md`, `canon/features/mcp-protocol/contracts/transport.md`

---

## Task 1: Implement JSON-RPC Conditions

**Files:**
- Modify: `src/conditions.lisp`
- Create: `tests/conditions-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/conditions-tests.lisp
;;; ABOUTME: Tests for MCP condition types

(in-package #:cl-mcp-server-tests)

(def-suite conditions-tests
  :description "Condition type tests"
  :in cl-mcp-server-tests)

(in-suite conditions-tests)

(test json-rpc-error-codes
  "JSON-RPC error conditions have correct codes"
  (is (= -32700 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:parse-error))))
  (is (= -32600 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:invalid-request))))
  (is (= -32601 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:method-not-found))))
  (is (= -32602 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:invalid-params))))
  (is (= -32603 (cl-mcp-server.conditions:error-code
                  (make-condition 'cl-mcp-server.conditions:internal-error)))))

(test json-rpc-error-messages
  "JSON-RPC error conditions have messages"
  (let ((err (make-condition 'cl-mcp-server.conditions:method-not-found
                             :message "Method foo not found")))
    (is (string= "Method foo not found"
                 (cl-mcp-server.conditions:error-message err)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::conditions-tests)"`
Expected: FAIL - conditions not defined

**Step 3: Implement conditions**

```lisp
;;; src/conditions.lisp
;;; ABOUTME: MCP and JSON-RPC condition definitions

(in-package #:cl-mcp-server.conditions)

;;; Base condition

(define-condition mcp-error (error)
  ((message :initarg :message :reader error-message :initform ""))
  (:documentation "Base condition for MCP errors"))

;;; JSON-RPC 2.0 standard error conditions

(define-condition json-rpc-error (mcp-error)
  ((code :reader error-code :allocation :class)
   (data :initarg :data :reader error-data :initform nil))
  (:documentation "Base condition for JSON-RPC errors"))

(define-condition parse-error (json-rpc-error)
  ((code :initform -32700 :allocation :class))
  (:report (lambda (c s)
             (format s "Parse error: ~a" (error-message c))))
  (:documentation "Invalid JSON was received"))

(define-condition invalid-request (json-rpc-error)
  ((code :initform -32600 :allocation :class))
  (:report (lambda (c s)
             (format s "Invalid Request: ~a" (error-message c))))
  (:documentation "The JSON sent is not a valid Request object"))

(define-condition method-not-found (json-rpc-error)
  ((code :initform -32601 :allocation :class))
  (:report (lambda (c s)
             (format s "Method not found: ~a" (error-message c))))
  (:documentation "The method does not exist / is not available"))

(define-condition invalid-params (json-rpc-error)
  ((code :initform -32602 :allocation :class))
  (:report (lambda (c s)
             (format s "Invalid params: ~a" (error-message c))))
  (:documentation "Invalid method parameter(s)"))

(define-condition internal-error (json-rpc-error)
  ((code :initform -32603 :allocation :class))
  (:report (lambda (c s)
             (format s "Internal error: ~a" (error-message c))))
  (:documentation "Internal JSON-RPC error"))
```

**Step 4: Add test file to system**

Update `cl-mcp-server.asd` test components:
```lisp
(:file "conditions-tests")  ; Add after packages
```

**Step 5: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::conditions-tests)"`
Expected: PASS

**Step 6: Commit**

```bash
git add src/conditions.lisp tests/conditions-tests.lisp cl-mcp-server.asd
git commit -m "feat: implement JSON-RPC error conditions"
```

---

## Task 2: Implement JSON-RPC Message Structures

**Files:**
- Modify: `src/json-rpc.lisp`
- Modify: `tests/json-rpc-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/json-rpc-tests.lisp (replace stub content)
;;; ABOUTME: Tests for JSON-RPC parsing and encoding

(in-package #:cl-mcp-server-tests)

(def-suite json-rpc-tests
  :description "JSON-RPC message tests"
  :in cl-mcp-server-tests)

(in-suite json-rpc-tests)

(test make-request
  "Can create JSON-RPC request objects"
  (let ((req (cl-mcp-server.json-rpc:make-request
               :id 1
               :method "test"
               :params '(("a" . 1)))))
    (is (= 1 (cl-mcp-server.json-rpc:request-id req)))
    (is (string= "test" (cl-mcp-server.json-rpc:request-method req)))
    (is (equal '(("a" . 1)) (cl-mcp-server.json-rpc:request-params req)))))

(test make-notification
  "Notifications have no ID"
  (let ((notif (cl-mcp-server.json-rpc:make-notification
                 :method "notify")))
    (is (null (cl-mcp-server.json-rpc:request-id notif)))
    (is (cl-mcp-server.json-rpc:notification-p notif))))

(test make-success-response
  "Can create success responses"
  (let ((resp (cl-mcp-server.json-rpc:make-success-response
                :id 1
                :result '(("value" . 42)))))
    (is (= 1 (cl-mcp-server.json-rpc:response-id resp)))
    (is (equal '(("value" . 42)) (cl-mcp-server.json-rpc:response-result resp)))
    (is (null (cl-mcp-server.json-rpc:response-error resp)))))

(test make-error-response
  "Can create error responses"
  (let ((resp (cl-mcp-server.json-rpc:make-error-response
                :id 1
                :code -32601
                :message "Method not found")))
    (is (= 1 (cl-mcp-server.json-rpc:response-id resp)))
    (is (= -32601 (getf (cl-mcp-server.json-rpc:response-error resp) :code)))
    (is (string= "Method not found"
                 (getf (cl-mcp-server.json-rpc:response-error resp) :message)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: FAIL - functions not defined

**Step 3: Implement message structures**

```lisp
;;; src/json-rpc.lisp
;;; ABOUTME: JSON-RPC 2.0 message parsing and encoding

(in-package #:cl-mcp-server.json-rpc)

;;; Message structures

(defstruct (json-rpc-request (:conc-name request-))
  "A JSON-RPC 2.0 request or notification"
  (id nil)           ; nil for notifications
  (method "" :type string)
  (params nil))      ; alist or list

(defstruct (json-rpc-response (:conc-name response-))
  "A JSON-RPC 2.0 response"
  (id nil)
  (result nil)
  (error nil))       ; plist (:code :message :data)

;;; Constructors

(defun make-request (&key id method params)
  "Create a JSON-RPC request"
  (make-json-rpc-request :id id :method method :params params))

(defun make-notification (&key method params)
  "Create a JSON-RPC notification (no id)"
  (make-json-rpc-request :id nil :method method :params params))

(defun notification-p (request)
  "True if request is a notification (no id)"
  (null (request-id request)))

(defun make-success-response (&key id result)
  "Create a successful JSON-RPC response"
  (make-json-rpc-response :id id :result result :error nil))

(defun make-error-response (&key id code message data)
  "Create an error JSON-RPC response"
  (make-json-rpc-response
    :id id
    :result nil
    :error (list :code code :message message :data data)))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/json-rpc.lisp tests/json-rpc-tests.lisp
git commit -m "feat: implement JSON-RPC message structures"
```

---

## Task 3: Implement JSON Parsing

**Files:**
- Modify: `src/json-rpc.lisp`
- Modify: `tests/json-rpc-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/json-rpc-tests.lisp`:

```lisp
(test parse-valid-request
  "Parse a valid JSON-RPC request"
  (let* ((json "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":1}}")
         (req (cl-mcp-server.json-rpc:parse-message json)))
    (is (= 1 (cl-mcp-server.json-rpc:request-id req)))
    (is (string= "test" (cl-mcp-server.json-rpc:request-method req)))))

(test parse-notification
  "Parse a notification (no id)"
  (let* ((json "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}")
         (req (cl-mcp-server.json-rpc:parse-message json)))
    (is (null (cl-mcp-server.json-rpc:request-id req)))
    (is (string= "notifications/initialized"
                 (cl-mcp-server.json-rpc:request-method req)))))

(test parse-invalid-json
  "Invalid JSON signals parse-error"
  (signals cl-mcp-server.conditions:parse-error
    (cl-mcp-server.json-rpc:parse-message "{not valid json}")))

(test parse-missing-jsonrpc
  "Missing jsonrpc field signals invalid-request"
  (signals cl-mcp-server.conditions:invalid-request
    (cl-mcp-server.json-rpc:parse-message "{\"id\":1,\"method\":\"test\"}")))

(test parse-missing-method
  "Missing method field signals invalid-request"
  (signals cl-mcp-server.conditions:invalid-request
    (cl-mcp-server.json-rpc:parse-message "{\"jsonrpc\":\"2.0\",\"id\":1}")))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: FAIL - parse-message not defined

**Step 3: Implement parsing**

Add to `src/json-rpc.lisp`:

```lisp
;;; Parsing

(defun parse-message (json-string)
  "Parse a JSON-RPC message from a JSON string.
   Signals parse-error for invalid JSON.
   Signals invalid-request for protocol violations.
   Returns a json-rpc-request struct."
  (let ((data (handler-case
                  (yason:parse json-string :object-as :alist)
                (error (e)
                  (error 'parse-error
                         :message (format nil "~a" e))))))
    (validate-and-build-request data)))

(defun validate-and-build-request (data)
  "Validate parsed JSON data and build request struct"
  (unless (assoc "jsonrpc" data :test #'string=)
    (error 'invalid-request :message "missing jsonrpc field"))
  (unless (string= "2.0" (cdr (assoc "jsonrpc" data :test #'string=)))
    (error 'invalid-request :message "jsonrpc must be \"2.0\""))
  (unless (assoc "method" data :test #'string=)
    (error 'invalid-request :message "missing method field"))
  (let ((method (cdr (assoc "method" data :test #'string=))))
    (unless (stringp method)
      (error 'invalid-request :message "method must be a string")))
  (make-json-rpc-request
    :id (cdr (assoc "id" data :test #'string=))
    :method (cdr (assoc "method" data :test #'string=))
    :params (cdr (assoc "params" data :test #'string=))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/json-rpc.lisp tests/json-rpc-tests.lisp
git commit -m "feat: implement JSON-RPC message parsing"
```

---

## Task 4: Implement JSON Encoding

**Files:**
- Modify: `src/json-rpc.lisp`
- Modify: `tests/json-rpc-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/json-rpc-tests.lisp`:

```lisp
(test encode-success-response
  "Encode success response to JSON"
  (let* ((resp (cl-mcp-server.json-rpc:make-success-response
                 :id 1
                 :result '(("value" . 42))))
         (json (cl-mcp-server.json-rpc:encode-response resp))
         (parsed (yason:parse json :object-as :alist)))
    (is (string= "2.0" (cdr (assoc "jsonrpc" parsed :test #'string=))))
    (is (= 1 (cdr (assoc "id" parsed :test #'string=))))
    (is (= 42 (cdr (assoc "value" (cdr (assoc "result" parsed :test #'string=))
                          :test #'string=))))
    ;; Must not have error field
    (is (not (assoc "error" parsed :test #'string=)))))

(test encode-error-response
  "Encode error response to JSON"
  (let* ((resp (cl-mcp-server.json-rpc:make-error-response
                 :id 1
                 :code -32601
                 :message "Method not found"))
         (json (cl-mcp-server.json-rpc:encode-response resp))
         (parsed (yason:parse json :object-as :alist)))
    (is (string= "2.0" (cdr (assoc "jsonrpc" parsed :test #'string=))))
    (is (= 1 (cdr (assoc "id" parsed :test #'string=))))
    ;; Must not have result field
    (is (not (assoc "result" parsed :test #'string=)))
    (let ((err (cdr (assoc "error" parsed :test #'string=))))
      (is (= -32601 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Method not found" (cdr (assoc "message" err :test #'string=)))))))

(test encode-null-id
  "Null id encodes as JSON null"
  (let* ((resp (cl-mcp-server.json-rpc:make-error-response
                 :id nil
                 :code -32700
                 :message "Parse error"))
         (json (cl-mcp-server.json-rpc:encode-response resp)))
    (is (search "\"id\":null" json))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: FAIL - encode-response not defined

**Step 3: Implement encoding**

Add to `src/json-rpc.lisp`:

```lisp
;;; Encoding

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table for yason encoding"
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht)
            (if (and (listp (cdr pair))
                     (consp (car (cdr pair)))
                     (stringp (caar (cdr pair))))
                (alist-to-hash-table (cdr pair))
                (cdr pair))))))

(defun encode-response (response)
  "Encode a JSON-RPC response to a JSON string.
   Returns a string with no trailing newline."
  (with-output-to-string (s)
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) (response-id response))
      (if (response-error response)
          ;; Error response
          (let ((err-ht (make-hash-table :test #'equal)))
            (setf (gethash "code" err-ht) (getf (response-error response) :code))
            (setf (gethash "message" err-ht) (getf (response-error response) :message))
            (when (getf (response-error response) :data)
              (setf (gethash "data" err-ht) (getf (response-error response) :data)))
            (setf (gethash "error" ht) err-ht))
          ;; Success response
          (setf (gethash "result" ht)
                (if (and (listp (response-result response))
                         (consp (car (response-result response)))
                         (stringp (caar (response-result response))))
                    (alist-to-hash-table (response-result response))
                    (response-result response))))
      (yason:encode ht s))))

(defun encode-error (condition &optional id)
  "Encode a json-rpc-error condition as a JSON response string"
  (encode-response
    (make-error-response
      :id id
      :code (error-code condition)
      :message (error-message condition)
      :data (when (slot-boundp condition 'data)
              (error-data condition)))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::json-rpc-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/json-rpc.lisp tests/json-rpc-tests.lisp
git commit -m "feat: implement JSON-RPC response encoding"
```

---

## Task 5: Implement Stdio Transport

**Files:**
- Modify: `src/transport.lisp`
- Modify: `tests/transport-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/transport-tests.lisp (replace stub)
;;; ABOUTME: Tests for stdio transport

(in-package #:cl-mcp-server-tests)

(def-suite transport-tests
  :description "Transport layer tests"
  :in cl-mcp-server-tests)

(in-suite transport-tests)

(test read-message-from-stream
  "Read a single JSON-RPC message from stream"
  (let* ((input "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}
")
         (stream (make-string-input-stream input)))
    (let ((msg (cl-mcp-server.transport:read-message stream)))
      (is (= 1 (cl-mcp-server.json-rpc:request-id msg)))
      (is (string= "test" (cl-mcp-server.json-rpc:request-method msg))))))

(test write-message-to-stream
  "Write a JSON-RPC response to stream"
  (let* ((resp (cl-mcp-server.json-rpc:make-success-response :id 1 :result t))
         (output (make-string-output-stream)))
    (cl-mcp-server.transport:write-message resp output)
    (let ((result (get-output-stream-string output)))
      ;; Should end with newline
      (is (char= #\Newline (char result (1- (length result)))))
      ;; Should be valid JSON
      (is (yason:parse result)))))

(test read-multiple-messages
  "Read multiple messages in sequence"
  (let* ((input (format nil "~a~%~a~%"
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"first\"}"
                        "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"second\"}"))
         (stream (make-string-input-stream input)))
    (let ((msg1 (cl-mcp-server.transport:read-message stream))
          (msg2 (cl-mcp-server.transport:read-message stream)))
      (is (string= "first" (cl-mcp-server.json-rpc:request-method msg1)))
      (is (string= "second" (cl-mcp-server.json-rpc:request-method msg2))))))

(test read-message-eof
  "EOF returns nil"
  (let ((stream (make-string-input-stream "")))
    (is (null (cl-mcp-server.transport:read-message stream)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::transport-tests)"`
Expected: FAIL - transport functions not defined

**Step 3: Implement transport**

```lisp
;;; src/transport.lisp
;;; ABOUTME: Stdio transport for MCP communication

(in-package #:cl-mcp-server.transport)

(defun read-message (&optional (stream *standard-input*))
  "Read a single JSON-RPC message from stream.
   Messages are newline-delimited JSON (NDJSON).
   Returns nil on EOF, json-rpc-request on success.
   Signals parse-error or invalid-request on bad input."
  (let ((line (read-line stream nil nil)))
    (when line
      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
        (unless (zerop (length trimmed))
          (cl-mcp-server.json-rpc:parse-message trimmed))))))

(defun write-message (response &optional (stream *standard-output*))
  "Write a JSON-RPC response to stream with newline delimiter."
  (write-string (cl-mcp-server.json-rpc:encode-response response) stream)
  (write-char #\Newline stream)
  (force-output stream))

(defmacro with-stdio-transport ((&key (input '*standard-input*)
                                      (output '*standard-output*))
                                &body body)
  "Execute body with stdio transport bindings.
   Useful for testing or redirecting I/O."
  `(let ((*standard-input* ,input)
         (*standard-output* ,output))
     ,@body))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::transport-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/transport.lisp tests/transport-tests.lisp
git commit -m "feat: implement stdio transport"
```

---

## Verification

After completing all tasks, run full test suite:

```bash
sbcl --noinform --non-interactive \
  --eval "(ql:quickload :cl-mcp-server/tests)" \
  --eval "(fiveam:run! 'cl-mcp-server-tests)"
```

Expected: All tests pass (conditions, json-rpc, transport)
