# MCP Server Integration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Integrate all components into a complete MCP server with tool definitions, request handling, and the main server loop.

**Architecture:** Tools defined as alist with name, description, schema, and handler. Server loop reads messages, dispatches to handlers, writes responses. Graceful shutdown on EOF.

**Tech Stack:** SBCL, FiveAM

**Dependencies:** All previous plans (00-04)

**Blocks:** None (final integration)

**Spec Reference:** `canon/features/mcp-protocol/contracts/`, `canon/features/mcp-protocol/scenarios/`

---

## Task 1: Implement Tool Definitions

**Files:**
- Modify: `src/tools.lisp`
- Create: `tests/tools-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/tools-tests.lisp
;;; ABOUTME: Tests for MCP tool definitions

(in-package #:cl-mcp-server-tests)

(def-suite tools-tests
  :description "MCP tools tests"
  :in cl-mcp-server-tests)

(in-suite tools-tests)

(test list-tools-returns-all
  "list-tools returns all defined tools"
  (let ((tools (cl-mcp-server.tools:list-tools)))
    (is (listp tools))
    (is (>= (length tools) 4))  ; At least 4 tools
    ;; Check required tools exist
    (is (find "evaluate-lisp" tools :key (lambda (t) (getf t :name)) :test #'string=))
    (is (find "list-definitions" tools :key (lambda (t) (getf t :name)) :test #'string=))
    (is (find "reset-session" tools :key (lambda (t) (getf t :name)) :test #'string=))
    (is (find "load-system" tools :key (lambda (t) (getf t :name)) :test #'string=))))

(test tool-has-required-fields
  "Each tool has name, description, inputSchema"
  (dolist (tool (cl-mcp-server.tools:list-tools))
    (is (getf tool :name))
    (is (getf tool :description))
    (is (getf tool :input-schema))))

(test get-tool-by-name
  "Can retrieve tool by name"
  (let ((tool (cl-mcp-server.tools:get-tool "evaluate-lisp")))
    (is tool)
    (is (string= "evaluate-lisp" (getf tool :name)))))

(test get-unknown-tool
  "Unknown tool returns nil"
  (is (null (cl-mcp-server.tools:get-tool "nonexistent-tool"))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::tools-tests)"`
Expected: FAIL - tools not defined

**Step 3: Implement tool definitions**

```lisp
;;; src/tools.lisp
;;; ABOUTME: MCP tool definitions and dispatch

(in-package #:cl-mcp-server.tools)

;;; Tool registry

(defparameter *tools* nil
  "List of available MCP tools")

(defun define-tool (name description input-schema handler)
  "Define an MCP tool"
  (let ((tool (list :name name
                    :description description
                    :input-schema input-schema
                    :handler handler)))
    (setf *tools* (cons tool (remove name *tools*
                                      :key (lambda (t) (getf t :name))
                                      :test #'string=)))
    tool))

(defun list-tools ()
  "Return list of all tools (without handlers)"
  (mapcar (lambda (tool)
            (list :name (getf tool :name)
                  :description (getf tool :description)
                  :input-schema (getf tool :input-schema)))
          *tools*))

(defun get-tool (name)
  "Get a tool definition by name"
  (find name *tools* :key (lambda (t) (getf t :name)) :test #'string=))

;;; Tool definitions

(define-tool "evaluate-lisp"
  "Evaluate Common Lisp code in a persistent REPL session. Definitions and variables persist across calls."
  '(("type" . "object")
    ("required" . ("code"))
    ("properties" . (("code" . (("type" . "string")
                                ("description" . "Common Lisp expression(s) to evaluate")))
                     ("package" . (("type" . "string")
                                   ("description" . "Package context for evaluation (default: CL-USER)"))))))
  (lambda (args session)
    (let ((code (cdr (assoc "code" args :test #'string=)))
          (pkg (cdr (assoc "package" args :test #'string=))))
      (when pkg
        (cl-mcp-server.session:switch-package pkg session))
      (let ((result (cl-mcp-server.evaluator:evaluate-code code session)))
        (values (cl-mcp-server.evaluator:format-result result)
                (not (cl-mcp-server.evaluator:result-success-p result)))))))

(define-tool "list-definitions"
  "List functions, variables, and other definitions in the current session."
  '(("type" . "object")
    ("properties" . (("type" . (("type" . "string")
                                ("enum" . ("all" "functions" "variables" "macros" "classes"))
                                ("description" . "Filter by definition type (default: all)"))))))
  (lambda (args session)
    (declare (ignore args))
    (values (cl-mcp-server.session:format-definitions session) nil)))

(define-tool "reset-session"
  "Clear all session state including definitions and variables. Start fresh."
  '(("type" . "object")
    ("properties" . ()))
  (lambda (args session)
    (declare (ignore args))
    (values (cl-mcp-server.session:reset-session session) nil)))

(define-tool "load-system"
  "Load an ASDF system using Quicklisp. The system becomes available for subsequent evaluations."
  '(("type" . "object")
    ("required" . ("system"))
    ("properties" . (("system" . (("type" . "string")
                                  ("description" . "ASDF system name to load (e.g., 'alexandria', 'cl-ppcre')"))))))
  (lambda (args session)
    (declare (ignore session))
    (let ((system-name (cdr (assoc "system" args :test #'string=))))
      (handler-case
          (progn
            (ql:quickload system-name :silent t)
            (values (format nil "Loaded: ~a" system-name) nil))
        (error (c)
          (values (format nil "[ERROR] ~a~%~a"
                          (type-of c) (princ-to-string c))
                  t))))))
```

Update package use clause in `src/packages.lisp`:
```lisp
(defpackage #:cl-mcp-server.tools
  (:use #:cl
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.session)
  (:export
   #:*tools*
   #:define-tool
   #:get-tool
   #:call-tool
   #:list-tools))
```

**Step 4: Add test file to system**

Update `cl-mcp-server.asd` test components:
```lisp
(:file "tools-tests")
```

**Step 5: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::tools-tests)"`
Expected: PASS

**Step 6: Commit**

```bash
git add src/tools.lisp src/packages.lisp tests/tools-tests.lisp cl-mcp-server.asd
git commit -m "feat: implement MCP tool definitions"
```

---

## Task 2: Implement Tool Calling

**Files:**
- Modify: `src/tools.lisp`
- Modify: `tests/tools-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/tools-tests.lisp`:

```lisp
(test call-evaluate-lisp
  "Call evaluate-lisp tool"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (multiple-value-bind (result is-error)
          (cl-mcp-server.tools:call-tool "evaluate-lisp"
                                          '(("code" . "(+ 1 2)"))
                                          session)
        (is (not is-error))
        (is (search "=> 3" result))))))

(test call-unknown-tool
  "Calling unknown tool signals error"
  (let ((session (cl-mcp-server.session:make-session)))
    (signals cl-mcp-server.conditions:invalid-params
      (cl-mcp-server.tools:call-tool "nonexistent" '() session))))

(test call-tool-missing-required
  "Missing required param signals error"
  (let ((session (cl-mcp-server.session:make-session)))
    (signals cl-mcp-server.conditions:invalid-params
      (cl-mcp-server.tools:call-tool "evaluate-lisp" '() session))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::tools-tests)"`
Expected: FAIL - call-tool not implemented

**Step 3: Implement call-tool**

Add to `src/tools.lisp`:

```lisp
;;; Tool invocation

(defun validate-tool-args (tool args)
  "Validate arguments against tool's input schema.
   Signals invalid-params on validation failure."
  (let* ((schema (getf tool :input-schema))
         (required (cdr (assoc "required" schema :test #'string=))))
    ;; Check required fields
    (dolist (req required)
      (unless (assoc req args :test #'string=)
        (error 'cl-mcp-server.conditions:invalid-params
               :message (format nil "missing required parameter '~a'" req))))))

(defun call-tool (name args &optional (session cl-mcp-server.session:*session*))
  "Call a tool by name with arguments.
   Returns two values: result text and is-error boolean.
   Signals invalid-params for unknown tool or validation failure."
  (let ((tool (get-tool name)))
    (unless tool
      (error 'cl-mcp-server.conditions:invalid-params
             :message (format nil "Unknown tool: ~a" name)))
    (validate-tool-args tool args)
    (funcall (getf tool :handler) args session)))
```

Update package use:
```lisp
(:use #:cl
      #:cl-mcp-server.conditions
      #:cl-mcp-server.evaluator
      #:cl-mcp-server.session)
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::tools-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/tools.lisp src/packages.lisp tests/tools-tests.lisp
git commit -m "feat: implement tool calling with validation"
```

---

## Task 3: Implement MCP Request Handlers

**Files:**
- Modify: `src/server.lisp`
- Modify: `tests/integration-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/integration-tests.lisp (replace stub)
;;; ABOUTME: End-to-end integration tests

(in-package #:cl-mcp-server-tests)

(def-suite integration-tests
  :description "Full MCP protocol integration tests"
  :in cl-mcp-server-tests)

(in-suite integration-tests)

(test handle-initialize
  "Handle initialize request"
  (let ((request (cl-mcp-server.json-rpc:make-request
                   :id 1
                   :method "initialize"
                   :params '(("protocolVersion" . "2025-03-26")
                             ("capabilities" . ())
                             ("clientInfo" . (("name" . "test") ("version" . "1.0")))))))
    (let ((response (cl-mcp-server::handle-request request)))
      (is (= 1 (cl-mcp-server.json-rpc:response-id response)))
      (is (null (cl-mcp-server.json-rpc:response-error response)))
      (let ((result (cl-mcp-server.json-rpc:response-result response)))
        (is (string= "2025-03-26" (cdr (assoc "protocolVersion" result :test #'string=))))))))

(test handle-tools-list
  "Handle tools/list request"
  (let ((request (cl-mcp-server.json-rpc:make-request
                   :id 2
                   :method "tools/list")))
    (let ((response (cl-mcp-server::handle-request request)))
      (is (= 2 (cl-mcp-server.json-rpc:response-id response)))
      (is (null (cl-mcp-server.json-rpc:response-error response)))
      (let* ((result (cl-mcp-server.json-rpc:response-result response))
             (tools (cdr (assoc "tools" result :test #'string=))))
        (is (listp tools))
        (is (>= (length tools) 4))))))

(test handle-tools-call
  "Handle tools/call request"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((request (cl-mcp-server.json-rpc:make-request
                       :id 3
                       :method "tools/call"
                       :params '(("name" . "evaluate-lisp")
                                 ("arguments" . (("code" . "(+ 1 2)")))))))
        (let ((response (cl-mcp-server::handle-request request)))
          (is (= 3 (cl-mcp-server.json-rpc:response-id response)))
          (is (null (cl-mcp-server.json-rpc:response-error response)))
          (let* ((result (cl-mcp-server.json-rpc:response-result response))
                 (content (cdr (assoc "content" result :test #'string=))))
            (is (listp content))
            (is (search "=> 3" (cdr (assoc "text" (first content) :test #'string=))))))))))

(test handle-unknown-method
  "Unknown method returns error"
  (let ((request (cl-mcp-server.json-rpc:make-request
                   :id 4
                   :method "unknown/method")))
    (let ((response (cl-mcp-server::handle-request request)))
      (is (= 4 (cl-mcp-server.json-rpc:response-id response)))
      (is (not (null (cl-mcp-server.json-rpc:response-error response))))
      (is (= -32601 (getf (cl-mcp-server.json-rpc:response-error response) :code))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::integration-tests)"`
Expected: FAIL - handle-request not implemented

**Step 3: Implement request handlers**

```lisp
;;; src/server.lisp
;;; ABOUTME: Main MCP server entry point

(in-package #:cl-mcp-server)

;;; Server configuration

(defparameter *protocol-version* "2025-03-26"
  "MCP protocol version")

(defparameter *server-info*
  '(("name" . "cl-mcp-server")
    ("version" . "0.1.0"))
  "Server identification")

(defparameter *server-capabilities*
  '(("tools" . ()))
  "Server capabilities for initialize response")

;;; Session state

(defvar *server-session* nil
  "Current server session")

(defvar *initialized* nil
  "Whether initialization handshake is complete")

;;; Request handlers

(defun handle-initialize (params id)
  "Handle initialize request"
  (declare (ignore params))
  (make-success-response
    :id id
    :result `(("protocolVersion" . ,*protocol-version*)
              ("capabilities" . ,*server-capabilities*)
              ("serverInfo" . ,*server-info*))))

(defun handle-initialized (request)
  "Handle initialized notification"
  (declare (ignore request))
  (setf *initialized* t)
  ;; Notifications don't get responses
  nil)

(defun handle-tools-list (params id)
  "Handle tools/list request"
  (declare (ignore params))
  (let ((tools (mapcar (lambda (tool)
                         `(("name" . ,(getf tool :name))
                           ("description" . ,(getf tool :description))
                           ("inputSchema" . ,(getf tool :input-schema))))
                       (list-tools))))
    (make-success-response
      :id id
      :result `(("tools" . ,tools)))))

(defun handle-tools-call (params id)
  "Handle tools/call request"
  (let ((name (cdr (assoc "name" params :test #'string=)))
        (args (cdr (assoc "arguments" params :test #'string=))))
    (handler-case
        (multiple-value-bind (result-text is-error)
            (call-tool name args *server-session*)
          (make-success-response
            :id id
            :result `(("content" . ((("type" . "text")
                                     ("text" . ,result-text))))
                      ("isError" . ,(if is-error t :false)))))
      (invalid-params (c)
        (make-error-response
          :id id
          :code (error-code c)
          :message (error-message c))))))

;;; Request dispatch

(defun handle-request (request)
  "Dispatch a request to the appropriate handler.
   Returns a response or nil for notifications."
  (let ((method (request-method request))
        (params (request-params request))
        (id (request-id request)))
    (cond
      ;; Notifications (no id)
      ((string= method "notifications/initialized")
       (handle-initialized request))
      ;; Requests
      ((string= method "initialize")
       (handle-initialize params id))
      ((string= method "tools/list")
       (handle-tools-list params id))
      ((string= method "tools/call")
       (handle-tools-call params id))
      ;; Unknown method
      (t
       (make-error-response
         :id id
         :code -32601
         :message (format nil "Method not found: ~a" method))))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::integration-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/server.lisp tests/integration-tests.lisp
git commit -m "feat: implement MCP request handlers"
```

---

## Task 4: Implement Server Main Loop

**Files:**
- Modify: `src/server.lisp`
- Modify: `tests/integration-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/integration-tests.lisp`:

```lisp
(test server-full-session
  "Full session: init, list, call, close"
  (let ((input (format nil "~a~%~a~%~a~%~a~%"
                       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-03-26\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test\",\"version\":\"1.0\"}}}"
                       "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
                       "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
                       "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"evaluate-lisp\",\"arguments\":{\"code\":\"(+ 40 2)\"}}}")))
    (let ((output (make-string-output-stream)))
      (with-input-from-string (in input)
        (cl-mcp-server:run-server :input in :output output))
      (let ((responses (get-output-stream-string output)))
        ;; Should have 3 responses (init doesn't generate one for notification)
        (is (search "\"id\":1" responses))  ; init response
        (is (search "\"id\":2" responses))  ; tools/list response
        (is (search "\"id\":3" responses))  ; tools/call response
        (is (search "=> 42" responses))))))  ; evaluation result
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::integration-tests)"`
Expected: FAIL - run-server not implemented

**Step 3: Implement server loop**

Add to `src/server.lisp`:

```lisp
;;; Main server loop

(defun run-server (&key (input *standard-input*) (output *standard-output*))
  "Run the MCP server.
   Reads JSON-RPC messages from INPUT, writes responses to OUTPUT.
   Returns on EOF or error."
  ;; Initialize session
  (setf *server-session* (make-session))
  (setf *initialized* nil)
  (with-session (*server-session*)
    (loop
      (let ((request (handler-case
                         (read-message input)
                       (error (c)
                         ;; Send parse error response
                         (write-message
                           (make-error-response
                             :id nil
                             :code -32700
                             :message (princ-to-string c))
                           output)
                         (continue)))))
        ;; EOF - exit cleanly
        (unless request
          (return))
        ;; Handle request
        (let ((response (handler-case
                            (handle-request request)
                          (json-rpc-error (c)
                            (make-error-response
                              :id (request-id request)
                              :code (error-code c)
                              :message (error-message c)))
                          (error (c)
                            (make-error-response
                              :id (request-id request)
                              :code -32603
                              :message (princ-to-string c))))))
          ;; Only send response for requests (not notifications)
          (when response
            (write-message response output)))))))

(defun start ()
  "Start the MCP server on stdio."
  (run-server))
```

Update package exports:
```lisp
#:start
#:run-server
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::integration-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/server.lisp src/packages.lisp tests/integration-tests.lisp
git commit -m "feat: implement main server loop"
```

---

## Task 5: Create Launcher Script

**Files:**
- Create: `run-server.lisp`

**Step 1: Write launcher script**

```lisp
#!/usr/bin/env sbcl --script
;;; run-server.lisp
;;; ABOUTME: Launcher script for CL-MCP-Server

;;; Load Quicklisp
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Load the server
(ql:quickload :cl-mcp-server :silent t)

;;; Run
(cl-mcp-server:start)
```

**Step 2: Test manually**

```bash
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | sbcl --load run-server.lisp
```

Expected: JSON response with server info

**Step 3: Commit**

```bash
git add run-server.lisp
git commit -m "feat: add launcher script"
```

---

## Verification

After completing all tasks, run full test suite:

```bash
sbcl --noinform --non-interactive \
  --eval "(ql:quickload :cl-mcp-server/tests)" \
  --eval "(fiveam:run! 'cl-mcp-server-tests)"
```

Expected: All tests pass

Test full server:

```bash
# Start server (in one terminal)
sbcl --load run-server.lisp

# Send test messages (in another terminal)
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(defun hello () \"Hello, MCP!\")"}}}
{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"evaluate-lisp","arguments":{"code":"(hello)"}}}' | sbcl --load run-server.lisp
```
