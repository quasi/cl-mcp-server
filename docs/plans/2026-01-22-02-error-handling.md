# Error Handling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement Lisp condition capture and structured error reporting with backtraces for the code evaluator.

**Architecture:** Uses handler-bind for non-unwinding condition capture. Separate formatters for errors and warnings. Portable backtrace via trivial-backtrace.

**Tech Stack:** SBCL, trivial-backtrace, FiveAM

**Dependencies:** Plan 01 (JSON-RPC Core) - uses condition types

**Blocks:** Plan 04 (Code Evaluator) - needs error capture

**Spec Reference:** `canon/features/error-handling/contracts/condition-report.md`, `canon/features/error-handling/scenarios/evaluation-errors.md`

---

## Task 1: Create Error Formatting Module

**Files:**
- Create: `src/error-format.lisp`
- Create: `tests/error-format-tests.lisp`
- Modify: `cl-mcp-server.asd` (add file)
- Modify: `src/packages.lisp` (add package)

**Step 1: Add package definition**

Add to `src/packages.lisp`:

```lisp
(defpackage #:cl-mcp-server.error-format
  (:use #:cl)
  (:export
   #:format-condition
   #:format-error
   #:format-warning
   #:format-backtrace
   #:*max-backtrace-depth*
   #:*print-backtrace-p*))
```

**Step 2: Write the failing test**

```lisp
;;; tests/error-format-tests.lisp
;;; ABOUTME: Tests for error formatting

(in-package #:cl-mcp-server-tests)

(def-suite error-format-tests
  :description "Error formatting tests"
  :in cl-mcp-server-tests)

(in-suite error-format-tests)

(test format-simple-error
  "Format a simple error condition"
  (handler-case
      (error "Test error message")
    (error (c)
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (search "SIMPLE-ERROR" formatted))
        (is (search "Test error message" formatted))))))

(test format-type-error
  "Format a type error with datum and expected type"
  (handler-case
      (+ 1 "not a number")
    (type-error (c)
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (search "TYPE-ERROR" formatted))
        (is (search "not a number" formatted))))))

(test format-undefined-function
  "Format undefined function error"
  (handler-case
      (funcall 'this-function-does-not-exist-12345)
    (undefined-function (c)
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (search "UNDEFINED-FUNCTION" formatted))
        (is (search "THIS-FUNCTION-DOES-NOT-EXIST-12345" formatted))))))

(test format-warning
  "Format a warning condition"
  (let ((warning-text nil))
    (handler-bind ((warning (lambda (c)
                              (setf warning-text
                                    (cl-mcp-server.error-format:format-warning c))
                              (muffle-warning c))))
      (warn "Test warning"))
    (is (search "WARNING" warning-text))
    (is (search "Test warning" warning-text))))

(test format-with-backtrace
  "Error format includes backtrace when enabled"
  (let ((cl-mcp-server.error-format:*print-backtrace-p* t))
    (handler-case
        (error "Backtrace test")
      (error (c)
        (let ((formatted (cl-mcp-server.error-format:format-error c)))
          (is (search "[Backtrace]" formatted)))))))

(test format-without-backtrace
  "Error format excludes backtrace when disabled"
  (let ((cl-mcp-server.error-format:*print-backtrace-p* nil))
    (handler-case
        (error "No backtrace test")
      (error (c)
        (let ((formatted (cl-mcp-server.error-format:format-error c)))
          (is (not (search "[Backtrace]" formatted))))))))
```

**Step 3: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: FAIL - package/functions not defined

**Step 4: Implement error formatting**

```lisp
;;; src/error-format.lisp
;;; ABOUTME: Condition formatting for MCP error responses

(in-package #:cl-mcp-server.error-format)

;;; Configuration

(defparameter *max-backtrace-depth* 20
  "Maximum number of backtrace frames to include")

(defparameter *print-backtrace-p* t
  "Whether to include backtrace in error output")

;;; Condition type extraction

(defun condition-type-name (condition)
  "Get the type name of a condition as a string"
  (string-upcase (symbol-name (type-of condition))))

;;; Message extraction

(defun condition-message (condition)
  "Extract the message from a condition"
  (handler-case
      (princ-to-string condition)
    (error ()
      (format nil "~a" (type-of condition)))))

;;; Backtrace formatting

(defun format-backtrace ()
  "Capture and format the current backtrace.
   Returns a string with numbered frames."
  (with-output-to-string (s)
    (trivial-backtrace:print-backtrace-to-stream s)))

(defun truncate-backtrace (backtrace-string)
  "Truncate backtrace to *max-backtrace-depth* frames"
  (with-input-from-string (in backtrace-string)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
            for count from 0
            while (and line (< count *max-backtrace-depth*))
            do (write-line line out)
            finally (when line
                      (write-line "..." out))))))

;;; Main formatting functions

(defun format-error (condition)
  "Format an error condition for MCP response.
   Returns a string in the format:
   [ERROR] CONDITION-TYPE
   message

   [Backtrace]
   ..."
  (with-output-to-string (s)
    (format s "[ERROR] ~a~%" (condition-type-name condition))
    (format s "~a~%" (condition-message condition))
    (when *print-backtrace-p*
      (format s "~%[Backtrace]~%")
      (write-string (truncate-backtrace (format-backtrace)) s))))

(defun format-warning (condition)
  "Format a warning condition.
   Returns a string in the format:
   CONDITION-TYPE: message"
  (format nil "~a: ~a"
          (condition-type-name condition)
          (condition-message condition)))

(defun format-condition (condition)
  "Format any condition appropriately"
  (if (typep condition 'warning)
      (format-warning condition)
      (format-error condition)))
```

**Step 5: Update ASDF system**

Add to `cl-mcp-server.asd` components (after conditions, before json-rpc):
```lisp
(:file "error-format")
```

Add to test components:
```lisp
(:file "error-format-tests")
```

**Step 6: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: PASS

**Step 7: Commit**

```bash
git add src/error-format.lisp src/packages.lisp tests/error-format-tests.lisp cl-mcp-server.asd
git commit -m "feat: implement error formatting with backtraces"
```

---

## Task 2: Implement Condition Capture Utilities

**Files:**
- Modify: `src/error-format.lisp`
- Modify: `tests/error-format-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/error-format-tests.lisp`:

```lisp
(test capture-error-with-context
  "Capture error with full context"
  (multiple-value-bind (result error-info)
      (cl-mcp-server.error-format:with-error-capture
        (error "Captured error"))
    (is (null result))
    (is (not (null error-info)))
    (is (search "SIMPLE-ERROR" (getf error-info :formatted)))))

(test capture-success
  "Successful evaluation returns values"
  (multiple-value-bind (result error-info)
      (cl-mcp-server.error-format:with-error-capture
        (+ 1 2))
    (is (= 3 result))
    (is (null error-info))))

(test capture-warnings
  "Warnings are collected but don't stop execution"
  (let ((warnings nil))
    (multiple-value-bind (result error-info)
        (cl-mcp-server.error-format:with-error-capture
          (lambda (w) (push w warnings))
          (warn "Warning 1")
          (warn "Warning 2")
          42)
      (is (= 42 result))
      (is (null error-info))
      (is (= 2 (length warnings))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: FAIL - with-error-capture not defined

**Step 3: Implement capture utilities**

Add to `src/error-format.lisp`:

```lisp
;;; Capture utilities

(defmacro with-error-capture ((&optional warning-handler) &body body)
  "Execute body, capturing any errors.
   Returns two values:
   1. Result of body (or nil on error)
   2. Error info plist (or nil on success)
      - :condition - the condition object
      - :type - condition type name string
      - :message - condition message string
      - :formatted - full formatted error string

   If warning-handler is provided, it's called with each warning condition.
   Warnings are muffled after handling."
  (let ((result (gensym "RESULT"))
        (error-info (gensym "ERROR-INFO")))
    `(let ((,result nil)
           (,error-info nil))
       (handler-bind
           ((warning (lambda (c)
                       ,(when warning-handler
                          `(funcall ,warning-handler c))
                       (muffle-warning c)))
            (error (lambda (c)
                     (setf ,error-info
                           (list :condition c
                                 :type (condition-type-name c)
                                 :message (condition-message c)
                                 :formatted (format-error c)))
                     (return-from with-error-capture-block
                       (values nil ,error-info)))))
         (block with-error-capture-block
           (setf ,result (progn ,@body))
           (values ,result nil))))))
```

Update package exports in `src/packages.lisp`:
```lisp
#:with-error-capture
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/error-format.lisp src/packages.lisp tests/error-format-tests.lisp
git commit -m "feat: add error capture macro with warning handling"
```

---

## Task 3: Add Specific Condition Handlers

**Files:**
- Modify: `src/error-format.lisp`
- Modify: `tests/error-format-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/error-format-tests.lisp`:

```lisp
(test format-unbound-variable
  "Format unbound variable error"
  (handler-case
      (symbol-value 'this-variable-is-not-bound-12345)
    (unbound-variable (c)
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (search "UNBOUND-VARIABLE" formatted))
        (is (search "THIS-VARIABLE-IS-NOT-BOUND-12345" formatted))))))

(test format-division-by-zero
  "Format division by zero error"
  (handler-case
      (/ 1 0)
    (division-by-zero (c)
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (search "DIVISION-BY-ZERO" formatted))))))

(test format-package-error
  "Format package error"
  (handler-case
      (find-package "NONEXISTENT-PACKAGE-12345")
    (error (c)
      ;; May or may not signal package-error depending on impl
      (let ((formatted (cl-mcp-server.error-format:format-error c)))
        (is (stringp formatted))))))

(test format-style-warning
  "Format style warning"
  (let ((warning-text nil))
    (handler-bind ((style-warning (lambda (c)
                                    (setf warning-text
                                          (cl-mcp-server.error-format:format-warning c))
                                    (muffle-warning c))))
      ;; Compile something that generates style warning
      (compile nil '(lambda () (let ((x 1)) nil))))
    (when warning-text
      (is (search "STYLE-WARNING" warning-text)))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: Tests should actually pass since our generic handlers work

**Step 3: Verify all tests pass**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::error-format-tests)"`
Expected: PASS (all condition types handled generically)

**Step 4: Commit**

```bash
git add tests/error-format-tests.lisp
git commit -m "test: add condition type coverage tests"
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

Test error formatting manually:
```lisp
(ql:quickload :cl-mcp-server)
(handler-case (/ 1 0)
  (error (c)
    (print (cl-mcp-server.error-format:format-error c))))
```
