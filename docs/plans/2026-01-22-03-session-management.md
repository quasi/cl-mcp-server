# Session Management Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement persistent REPL session state tracking with definition listing, package context, and session reset capabilities.

**Architecture:** Session struct holding package reference. Uses CL-USER as sandbox package. Tracks definitions via package symbol iteration. Reset by uninterning user symbols.

**Tech Stack:** SBCL, FiveAM

**Dependencies:** Plan 01 (JSON-RPC Core)

**Blocks:** Plan 04 (Code Evaluator) - needs session context

**Spec Reference:** `canon/features/session-management/contracts/session-state.md`, `canon/features/session-management/scenarios/state-persistence.md`

---

## Task 1: Implement Session Structure

**Files:**
- Modify: `src/session.lisp`
- Modify: `tests/session-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/session-tests.lisp (replace stub)
;;; ABOUTME: Tests for session management

(in-package #:cl-mcp-server-tests)

(def-suite session-tests
  :description "Session management tests"
  :in cl-mcp-server-tests)

(in-suite session-tests)

(test make-session
  "Can create a session with default package"
  (let ((session (cl-mcp-server.session:make-session)))
    (is (not (null session)))
    (is (eq (find-package :cl-user)
            (cl-mcp-server.session:session-package session)))))

(test session-with-custom-package
  "Can create session with custom package"
  (unwind-protect
      (progn
        (make-package :test-session-pkg-12345 :use '(:cl))
        (let ((session (cl-mcp-server.session:make-session
                         :package (find-package :test-session-pkg-12345))))
          (is (eq (find-package :test-session-pkg-12345)
                  (cl-mcp-server.session:session-package session)))))
    (delete-package :test-session-pkg-12345)))

(test session-special-variable
  "*session* is bound during with-session"
  (is (null cl-mcp-server.session:*session*))
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (is (eq session cl-mcp-server.session:*session*)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: FAIL - session not implemented

**Step 3: Implement session structure**

```lisp
;;; src/session.lisp
;;; ABOUTME: Session state management for persistent REPL

(in-package #:cl-mcp-server.session)

;;; Session state

(defvar *session* nil
  "The current session, bound during evaluation")

(defstruct (session (:constructor %make-session))
  "A REPL session with persistent state"
  (package (find-package :cl-user) :type package)
  (loaded-systems nil :type list))

(defun make-session (&key (package (find-package :cl-user)))
  "Create a new session with optional package context"
  (%make-session :package (if (packagep package)
                               package
                               (find-package package))))

(defmacro with-session ((session) &body body)
  "Execute body with *session* bound to session"
  `(let ((*session* ,session))
     ,@body))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/session.lisp tests/session-tests.lisp
git commit -m "feat: implement basic session structure"
```

---

## Task 2: Implement Definition Listing

**Files:**
- Modify: `src/session.lisp`
- Modify: `tests/session-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/session-tests.lisp`:

```lisp
(test list-definitions-empty
  "Empty session has no definitions"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      ;; Create a fresh package for isolation
      (unwind-protect
          (progn
            (make-package :test-defs-pkg-12345 :use '(:cl))
            (setf (cl-mcp-server.session:session-package session)
                  (find-package :test-defs-pkg-12345))
            (let ((defs (cl-mcp-server.session:list-definitions)))
              (is (null (getf defs :functions)))
              (is (null (getf defs :variables)))
              (is (null (getf defs :macros)))))
        (delete-package :test-defs-pkg-12345)))))

(test list-definitions-with-function
  "List definitions shows defined function"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (unwind-protect
          (progn
            (make-package :test-defs-fn-pkg-12345 :use '(:cl))
            (setf (cl-mcp-server.session:session-package session)
                  (find-package :test-defs-fn-pkg-12345))
            ;; Define a function in the test package
            (eval '(defun test-defs-fn-pkg-12345::my-test-fn (x) (* x 2)))
            (let ((defs (cl-mcp-server.session:list-definitions)))
              (is (member 'test-defs-fn-pkg-12345::my-test-fn
                          (getf defs :functions)))))
        (delete-package :test-defs-fn-pkg-12345)))))

(test list-definitions-with-variable
  "List definitions shows defined variable"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (unwind-protect
          (progn
            (make-package :test-defs-var-pkg-12345 :use '(:cl))
            (setf (cl-mcp-server.session:session-package session)
                  (find-package :test-defs-var-pkg-12345))
            (eval '(defvar test-defs-var-pkg-12345::*my-test-var* 42))
            (let ((defs (cl-mcp-server.session:list-definitions)))
              (is (member 'test-defs-var-pkg-12345::*my-test-var*
                          (getf defs :variables)))))
        (delete-package :test-defs-var-pkg-12345)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: FAIL - list-definitions not implemented

**Step 3: Implement list-definitions**

Add to `src/session.lisp`:

```lisp
;;; Definition tracking

(defun list-definitions (&optional (session *session*))
  "List all user definitions in the session's package.
   Returns a plist with :functions, :variables, :macros, :classes keys."
  (let ((pkg (session-package session))
        (functions nil)
        (variables nil)
        (macros nil)
        (classes nil))
    (do-symbols (sym pkg)
      (when (eq (symbol-package sym) pkg)
        (cond
          ;; Check macro first (macros are also fboundp)
          ((macro-function sym)
           (push sym macros))
          ;; Regular function
          ((fboundp sym)
           (push sym functions))
          ;; Special variable or constant
          ((boundp sym)
           (push sym variables)))
        ;; Check for class (can coexist with function)
        (when (find-class sym nil)
          (push sym classes))))
    (list :functions (nreverse functions)
          :variables (nreverse variables)
          :macros (nreverse macros)
          :classes (nreverse classes))))

(defun format-definitions (&optional (session *session*))
  "Format definitions for display.
   Returns a human-readable string."
  (let ((defs (list-definitions session)))
    (with-output-to-string (s)
      (flet ((format-section (name items formatter)
               (when items
                 (format s "[~a]~%" name)
                 (dolist (item items)
                   (format s "- ~a~%" (funcall formatter item)))
                 (terpri s))))
        (format-section "Functions" (getf defs :functions)
                        (lambda (fn)
                          (let ((arglist (sb-introspect:function-lambda-list fn)))
                            (format nil "~a ~a" fn arglist))))
        (format-section "Variables" (getf defs :variables)
                        (lambda (var)
                          (format nil "~a = ~s" var (symbol-value var))))
        (format-section "Macros" (getf defs :macros)
                        (lambda (mac)
                          (let ((arglist (sb-introspect:function-lambda-list
                                           (macro-function mac))))
                            (format nil "~a ~a" mac arglist))))
        (format-section "Classes" (getf defs :classes)
                        #'symbol-name))
      (when (and (null (getf defs :functions))
                 (null (getf defs :variables))
                 (null (getf defs :macros))
                 (null (getf defs :classes)))
        (write-string "No user definitions." s)))))
```

Update package exports:
```lisp
#:list-definitions
#:format-definitions
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/session.lisp src/packages.lisp tests/session-tests.lisp
git commit -m "feat: implement definition listing"
```

---

## Task 3: Implement Session Reset

**Files:**
- Modify: `src/session.lisp`
- Modify: `tests/session-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/session-tests.lisp`:

```lisp
(test reset-session-clears-definitions
  "Reset session clears all user definitions"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (unwind-protect
          (progn
            (make-package :test-reset-pkg-12345 :use '(:cl))
            (setf (cl-mcp-server.session:session-package session)
                  (find-package :test-reset-pkg-12345))
            ;; Create some definitions
            (eval '(defun test-reset-pkg-12345::my-fn () 1))
            (eval '(defvar test-reset-pkg-12345::*my-var* 2))
            ;; Verify they exist
            (let ((defs (cl-mcp-server.session:list-definitions)))
              (is (not (null (getf defs :functions))))
              (is (not (null (getf defs :variables)))))
            ;; Reset
            (cl-mcp-server.session:reset-session)
            ;; Verify cleared
            (let ((defs (cl-mcp-server.session:list-definitions)))
              (is (null (getf defs :functions)))
              (is (null (getf defs :variables)))))
        (ignore-errors (delete-package :test-reset-pkg-12345))))))

(test reset-session-preserves-package
  "Reset session keeps the same package"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (unwind-protect
          (progn
            (make-package :test-reset-preserve-12345 :use '(:cl))
            (setf (cl-mcp-server.session:session-package session)
                  (find-package :test-reset-preserve-12345))
            (cl-mcp-server.session:reset-session)
            (is (eq (find-package :test-reset-preserve-12345)
                    (cl-mcp-server.session:session-package session))))
        (ignore-errors (delete-package :test-reset-preserve-12345))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: FAIL - reset-session not implemented

**Step 3: Implement reset-session**

Add to `src/session.lisp`:

```lisp
;;; Session reset

(defun reset-session (&optional (session *session*))
  "Clear all user definitions from the session.
   Uninterns all symbols owned by the session package.
   Preserves the package itself and loaded systems."
  (let ((pkg (session-package session)))
    ;; Collect symbols first to avoid modifying while iterating
    (let ((symbols-to-unintern nil))
      (do-symbols (sym pkg)
        (when (eq (symbol-package sym) pkg)
          (push sym symbols-to-unintern)))
      ;; Now unintern them
      (dolist (sym symbols-to-unintern)
        ;; Remove function binding
        (when (fboundp sym)
          (fmakunbound sym))
        ;; Unintern the symbol
        (unintern sym pkg))))
  ;; Return confirmation message
  (format nil "Session reset. All definitions cleared.~%Current package: ~a"
          (package-name (session-package session))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/session.lisp tests/session-tests.lisp
git commit -m "feat: implement session reset"
```

---

## Task 4: Implement Package Switching

**Files:**
- Modify: `src/session.lisp`
- Modify: `tests/session-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/session-tests.lisp`:

```lisp
(test switch-package
  "Can switch session package"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (unwind-protect
          (progn
            (make-package :test-switch-pkg-12345 :use '(:cl))
            (cl-mcp-server.session:switch-package :test-switch-pkg-12345)
            (is (eq (find-package :test-switch-pkg-12345)
                    (cl-mcp-server.session:session-package session))))
        (ignore-errors (delete-package :test-switch-pkg-12345))))))

(test switch-to-nonexistent-package
  "Switching to nonexistent package signals error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (signals cl-mcp-server.conditions:mcp-error
        (cl-mcp-server.session:switch-package :nonexistent-pkg-12345)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: FAIL - switch-package not implemented

**Step 3: Implement switch-package**

Add to `src/session.lisp`:

```lisp
;;; Package management

(defun switch-package (package-designator &optional (session *session*))
  "Switch the session's current package.
   PACKAGE-DESIGNATOR can be a package, string, or symbol.
   Signals mcp-error if package doesn't exist."
  (let ((pkg (find-package package-designator)))
    (unless pkg
      (error 'cl-mcp-server.conditions:mcp-error
             :message (format nil "Package ~s not found" package-designator)))
    (setf (session-package session) pkg)
    pkg))
```

Update package use:
```lisp
(:use #:cl #:cl-mcp-server.conditions)
```

And exports:
```lisp
#:switch-package
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::session-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/session.lisp src/packages.lisp tests/session-tests.lisp
git commit -m "feat: implement package switching"
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

Test session interactively:
```lisp
(ql:quickload :cl-mcp-server)
(let ((s (cl-mcp-server.session:make-session)))
  (cl-mcp-server.session:with-session (s)
    (print (cl-mcp-server.session:list-definitions))
    (eval '(defun foo () 42))
    (print (cl-mcp-server.session:list-definitions))
    (cl-mcp-server.session:reset-session)
    (print (cl-mcp-server.session:list-definitions))))
```
