# Project Bootstrap Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Set up the Common Lisp project structure with ASDF system definition, package hierarchy, and dependencies.

**Architecture:** Standard ASDF project layout with src/ for source, tests/ for FiveAM tests. Single system with multiple components. Quicklisp for dependency management.

**Tech Stack:** SBCL, ASDF, Quicklisp, FiveAM (testing), yason (JSON)

**Dependencies:** None (this is the first plan)

**Blocks:** All other plans

---

## Task 1: Create ASDF System Definition

**Files:**
- Create: `cl-mcp-server.asd`

**Step 1: Write the system definition file**

```lisp
;;; cl-mcp-server.asd
;;; ABOUTME: ASDF system definition for CL-MCP-Server

(asdf:defsystem #:cl-mcp-server
  :description "Model Context Protocol server for Common Lisp evaluation"
  :author "Baba <quasi@quasilabs.com>"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (#:yason           ; JSON parsing
               #:alexandria      ; Utilities
               #:bordeaux-threads ; Threading (future)
               #:trivial-backtrace) ; Portable backtraces
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "conditions")
                 (:file "json-rpc")
                 (:file "transport")
                 (:file "session")
                 (:file "evaluator")
                 (:file "tools")
                 (:file "server"))))
  :in-order-to ((test-op (test-op #:cl-mcp-server/tests))))

(asdf:defsystem #:cl-mcp-server/tests
  :description "Tests for CL-MCP-Server"
  :depends-on (#:cl-mcp-server
               #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "json-rpc-tests")
                 (:file "transport-tests")
                 (:file "session-tests")
                 (:file "evaluator-tests")
                 (:file "integration-tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :cl-mcp-server-tests)))
```

**Step 2: Verify syntax**

Run: `sbcl --noinform --non-interactive --eval "(asdf:load-asd \"cl-mcp-server.asd\")"`
Expected: No errors (system won't load yet, but definition is valid)

**Step 3: Commit**

```bash
git add cl-mcp-server.asd
git commit -m "chore: add ASDF system definition"
```

---

## Task 2: Create Package Definitions

**Files:**
- Create: `src/packages.lisp`

**Step 1: Write package definitions**

```lisp
;;; src/packages.lisp
;;; ABOUTME: Package definitions for CL-MCP-Server

(defpackage #:cl-mcp-server.conditions
  (:use #:cl)
  (:export
   ;; Condition types
   #:mcp-error
   #:json-rpc-error
   #:parse-error
   #:invalid-request
   #:method-not-found
   #:invalid-params
   #:internal-error
   ;; Condition accessors
   #:error-code
   #:error-message
   #:error-data))

(defpackage #:cl-mcp-server.json-rpc
  (:use #:cl #:cl-mcp-server.conditions)
  (:export
   ;; Message types
   #:json-rpc-request
   #:json-rpc-response
   #:json-rpc-error-response
   #:json-rpc-notification
   ;; Accessors
   #:request-id
   #:request-method
   #:request-params
   #:response-id
   #:response-result
   #:response-error
   ;; Functions
   #:parse-message
   #:encode-response
   #:encode-error
   #:make-success-response
   #:make-error-response))

(defpackage #:cl-mcp-server.transport
  (:use #:cl #:cl-mcp-server.json-rpc)
  (:export
   #:read-message
   #:write-message
   #:with-stdio-transport))

(defpackage #:cl-mcp-server.session
  (:use #:cl)
  (:export
   #:*session*
   #:session
   #:make-session
   #:session-package
   #:session-definitions
   #:reset-session
   #:list-definitions
   #:with-session))

(defpackage #:cl-mcp-server.evaluator
  (:use #:cl #:cl-mcp-server.session)
  (:export
   #:evaluate-code
   #:evaluation-result
   #:result-values
   #:result-stdout
   #:result-stderr
   #:result-warnings
   #:result-error
   #:result-success-p))

(defpackage #:cl-mcp-server.tools
  (:use #:cl
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.session)
  (:export
   #:*tools*
   #:tool-definition
   #:get-tool
   #:call-tool
   #:list-tools))

(defpackage #:cl-mcp-server
  (:use #:cl
        #:cl-mcp-server.conditions
        #:cl-mcp-server.json-rpc
        #:cl-mcp-server.transport
        #:cl-mcp-server.session
        #:cl-mcp-server.evaluator
        #:cl-mcp-server.tools)
  (:export
   #:start
   #:*server-info*))
```

**Step 2: Verify syntax**

Run: `sbcl --noinform --non-interactive --load src/packages.lisp --eval "(print 'ok)"`
Expected: Prints `OK`

**Step 3: Commit**

```bash
git add src/packages.lisp
git commit -m "feat: add package definitions"
```

---

## Task 3: Create Directory Structure and Stub Files

**Files:**
- Create: `src/conditions.lisp`
- Create: `src/json-rpc.lisp`
- Create: `src/transport.lisp`
- Create: `src/session.lisp`
- Create: `src/evaluator.lisp`
- Create: `src/tools.lisp`
- Create: `src/server.lisp`
- Create: `tests/packages.lisp`

**Step 1: Create stub source files**

```lisp
;;; src/conditions.lisp
;;; ABOUTME: MCP and JSON-RPC condition definitions

(in-package #:cl-mcp-server.conditions)

;; Stub - to be implemented in error-handling plan
```

```lisp
;;; src/json-rpc.lisp
;;; ABOUTME: JSON-RPC 2.0 message parsing and encoding

(in-package #:cl-mcp-server.json-rpc)

;; Stub - to be implemented in json-rpc plan
```

```lisp
;;; src/transport.lisp
;;; ABOUTME: Stdio transport for MCP communication

(in-package #:cl-mcp-server.transport)

;; Stub - to be implemented in mcp-protocol plan
```

```lisp
;;; src/session.lisp
;;; ABOUTME: Session state management for persistent REPL

(in-package #:cl-mcp-server.session)

;; Stub - to be implemented in session-management plan
```

```lisp
;;; src/evaluator.lisp
;;; ABOUTME: Common Lisp code evaluation with output capture

(in-package #:cl-mcp-server.evaluator)

;; Stub - to be implemented in code-evaluator plan
```

```lisp
;;; src/tools.lisp
;;; ABOUTME: MCP tool definitions and dispatch

(in-package #:cl-mcp-server.tools)

;; Stub - to be implemented in mcp-server plan
```

```lisp
;;; src/server.lisp
;;; ABOUTME: Main MCP server entry point

(in-package #:cl-mcp-server)

(defparameter *server-info*
  '(("name" . "cl-mcp-server")
    ("version" . "0.1.0"))
  "Server identification for MCP initialize response.")

(defun start ()
  "Start the MCP server. Reads from stdin, writes to stdout."
  (error "Not yet implemented"))
```

**Step 2: Create test package**

```lisp
;;; tests/packages.lisp
;;; ABOUTME: Test package definitions

(defpackage #:cl-mcp-server-tests
  (:use #:cl #:fiveam)
  (:export #:run-all-tests))

(in-package #:cl-mcp-server-tests)

(def-suite cl-mcp-server-tests
  :description "All tests for CL-MCP-Server")

(defun run-all-tests ()
  (run! 'cl-mcp-server-tests))
```

**Step 3: Verify system loads**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server)" --eval "(print 'loaded)"`
Expected: Prints `LOADED` (with dependency loading messages)

**Step 4: Commit**

```bash
git add src/ tests/
git commit -m "feat: add project structure with stub files"
```

---

## Task 4: Create Test Stub Files

**Files:**
- Create: `tests/json-rpc-tests.lisp`
- Create: `tests/transport-tests.lisp`
- Create: `tests/session-tests.lisp`
- Create: `tests/evaluator-tests.lisp`
- Create: `tests/integration-tests.lisp`

**Step 1: Create test stubs**

```lisp
;;; tests/json-rpc-tests.lisp
;;; ABOUTME: Tests for JSON-RPC parsing and encoding

(in-package #:cl-mcp-server-tests)

(def-suite json-rpc-tests
  :description "JSON-RPC message tests"
  :in cl-mcp-server-tests)

;; Tests to be added in json-rpc plan
```

```lisp
;;; tests/transport-tests.lisp
;;; ABOUTME: Tests for stdio transport

(in-package #:cl-mcp-server-tests)

(def-suite transport-tests
  :description "Transport layer tests"
  :in cl-mcp-server-tests)

;; Tests to be added in mcp-protocol plan
```

```lisp
;;; tests/session-tests.lisp
;;; ABOUTME: Tests for session management

(in-package #:cl-mcp-server-tests)

(def-suite session-tests
  :description "Session management tests"
  :in cl-mcp-server-tests)

;; Tests to be added in session-management plan
```

```lisp
;;; tests/evaluator-tests.lisp
;;; ABOUTME: Tests for code evaluator

(in-package #:cl-mcp-server-tests)

(def-suite evaluator-tests
  :description "Code evaluator tests"
  :in cl-mcp-server-tests)

;; Tests to be added in code-evaluator plan
```

```lisp
;;; tests/integration-tests.lisp
;;; ABOUTME: End-to-end integration tests

(in-package #:cl-mcp-server-tests)

(def-suite integration-tests
  :description "Full MCP protocol integration tests"
  :in cl-mcp-server-tests)

;; Tests to be added in mcp-server plan
```

**Step 2: Verify tests run (empty but no errors)**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(asdf:test-system :cl-mcp-server)"`
Expected: Test run completes with 0 tests (no failures)

**Step 3: Commit**

```bash
git add tests/
git commit -m "feat: add test suite structure"
```

---

## Task 5: Create .gitignore

**Files:**
- Create: `.gitignore`

**Step 1: Write gitignore**

```
# Lisp compiled files
*.fasl
*.fas
*.lib
*.o

# SBCL specific
*.core

# Editor backups
*~
\#*\#
.\#*

# macOS
.DS_Store

# Local configuration
local.lisp
```

**Step 2: Commit**

```bash
git add .gitignore
git commit -m "chore: add .gitignore"
```

---

## Verification

After completing all tasks:

1. System loads: `(ql:quickload :cl-mcp-server)`
2. Tests run: `(asdf:test-system :cl-mcp-server)`
3. All packages exist and export symbols

The project is now ready for feature implementation.
