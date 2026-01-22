# Code Evaluator Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement Common Lisp code evaluation with output capture, multiple value handling, and condition handling for the MCP server.

**Architecture:** Evaluate code in session package context. Capture stdout/stderr via stream rebinding. Use handler-bind for warnings (muffle), handler-case for errors. Return structured evaluation-result.

**Tech Stack:** SBCL, FiveAM

**Dependencies:** Plan 02 (Error Handling), Plan 03 (Session Management)

**Blocks:** Plan 05 (MCP Server Integration)

**Spec Reference:** `canon/features/code-evaluator/contracts/evaluate-lisp-tool.md`, `canon/features/code-evaluator/scenarios/`

---

## Task 1: Implement Evaluation Result Structure

**Files:**
- Modify: `src/evaluator.lisp`
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

```lisp
;;; tests/evaluator-tests.lisp (replace stub)
;;; ABOUTME: Tests for code evaluator

(in-package #:cl-mcp-server-tests)

(def-suite evaluator-tests
  :description "Code evaluator tests"
  :in cl-mcp-server-tests)

(in-suite evaluator-tests)

(test evaluation-result-success
  "Can create success evaluation result"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                  :success-p t
                  :values '("42")
                  :stdout ""
                  :stderr ""
                  :warnings nil
                  :error-info nil)))
    (is (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("42") (cl-mcp-server.evaluator:result-values result)))
    (is (string= "" (cl-mcp-server.evaluator:result-stdout result)))))

(test evaluation-result-error
  "Can create error evaluation result"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                  :success-p nil
                  :values nil
                  :stdout ""
                  :stderr ""
                  :warnings nil
                  :error-info '(:type "ERROR" :message "test error"))))
    (is (not (cl-mcp-server.evaluator:result-success-p result)))
    (is (not (null (cl-mcp-server.evaluator:result-error result))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: FAIL - result structure not defined

**Step 3: Implement result structure**

```lisp
;;; src/evaluator.lisp
;;; ABOUTME: Common Lisp code evaluation with output capture

(in-package #:cl-mcp-server.evaluator)

;;; Evaluation result

(defstruct (evaluation-result (:conc-name result-))
  "Result of evaluating Lisp code"
  (success-p nil :type boolean)
  (values nil :type list)       ; List of printed value strings
  (stdout "" :type string)
  (stderr "" :type string)
  (warnings nil :type list)     ; List of warning strings
  (error nil))                  ; Error info plist or nil

(defun make-evaluation-result (&key success-p values stdout stderr warnings error-info)
  "Create an evaluation result"
  (make-evaluation-result
    :success-p success-p
    :values values
    :stdout (or stdout "")
    :stderr (or stderr "")
    :warnings (or warnings nil)
    :error (or error-info nil)))
```

Wait, there's a naming conflict. Let me fix:

```lisp
;;; src/evaluator.lisp
;;; ABOUTME: Common Lisp code evaluation with output capture

(in-package #:cl-mcp-server.evaluator)

;;; Evaluation result

(defstruct (evaluation-result (:conc-name result-)
                              (:constructor %make-evaluation-result))
  "Result of evaluating Lisp code"
  (success-p nil :type boolean)
  (values nil :type list)       ; List of printed value strings
  (stdout "" :type string)
  (stderr "" :type string)
  (warnings nil :type list)     ; List of warning strings
  (error nil))                  ; Error info plist or nil

(defun make-evaluation-result (&key success-p values stdout stderr warnings error-info)
  "Create an evaluation result"
  (%make-evaluation-result
    :success-p success-p
    :values values
    :stdout (or stdout "")
    :stderr (or stderr "")
    :warnings (or warnings nil)
    :error (or error-info nil)))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/evaluator.lisp tests/evaluator-tests.lisp
git commit -m "feat: implement evaluation result structure"
```

---

## Task 2: Implement Basic Code Evaluation

**Files:**
- Modify: `src/evaluator.lisp`
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/evaluator-tests.lisp`:

```lisp
(test evaluate-simple-expression
  "Evaluate a simple arithmetic expression"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2 3)")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (equal '("6") (cl-mcp-server.evaluator:result-values result)))))))

(test evaluate-multiple-values
  "Evaluate expression returning multiple values"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(floor 17 5)")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (equal '("3" "2") (cl-mcp-server.evaluator:result-values result)))))))

(test evaluate-nil-result
  "Evaluate expression returning NIL"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(member 'x '(a b c))")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (equal '("NIL") (cl-mcp-server.evaluator:result-values result)))))))

(test evaluate-multiple-forms
  "Evaluate multiple forms, return last result"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 1) (+ 2 2) (+ 3 3)")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (equal '("6") (cl-mcp-server.evaluator:result-values result)))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: FAIL - evaluate-code not implemented

**Step 3: Implement evaluate-code**

Add to `src/evaluator.lisp`:

```lisp
;;; Print settings for readable output

(defparameter *print-length-limit* 100
  "Maximum list elements to print")

(defparameter *print-level-limit* 10
  "Maximum nesting depth to print")

;;; Value formatting

(defun format-value (value)
  "Format a value for display, respecting print limits"
  (let ((*print-length* *print-length-limit*)
        (*print-level* *print-level-limit*)
        (*print-circle* t)
        (*print-pretty* t))
    (prin1-to-string value)))

(defun format-values (values)
  "Format multiple values as a list of strings"
  (mapcar #'format-value values))

;;; Code reading

(defun read-all-forms (string)
  "Read all Lisp forms from a string.
   Returns a list of forms."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil stream)
          until (eq form stream)
          collect form)))

;;; Core evaluation

(defun evaluate-code (code-string &optional (session cl-mcp-server.session:*session*))
  "Evaluate Common Lisp code in the session context.
   Returns an evaluation-result struct."
  (let* ((pkg (cl-mcp-server.session:session-package session))
         (*package* pkg)
         (forms (handler-case
                    (read-all-forms code-string)
                  (error (c)
                    (return-from evaluate-code
                      (make-evaluation-result
                        :success-p nil
                        :error-info (list :type (symbol-name (type-of c))
                                          :message (princ-to-string c)
                                          :formatted (cl-mcp-server.error-format:format-error c))))))))
    ;; Evaluate all forms, keeping only last result
    (let ((result-values nil))
      (handler-case
          (progn
            (dolist (form forms)
              (setf result-values (multiple-value-list (eval form))))
            (make-evaluation-result
              :success-p t
              :values (format-values result-values)))
        (error (c)
          (make-evaluation-result
            :success-p nil
            :error-info (list :type (symbol-name (type-of c))
                              :message (princ-to-string c)
                              :formatted (cl-mcp-server.error-format:format-error c))))))))
```

Update package use clause:
```lisp
(:use #:cl #:cl-mcp-server.session #:cl-mcp-server.error-format)
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/evaluator.lisp src/packages.lisp tests/evaluator-tests.lisp
git commit -m "feat: implement basic code evaluation"
```

---

## Task 3: Implement Output Capture

**Files:**
- Modify: `src/evaluator.lisp`
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/evaluator-tests.lisp`:

```lisp
(test evaluate-with-stdout
  "Capture standard output"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(format t \"Hello, World!~%\") 42")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (search "Hello, World!" (cl-mcp-server.evaluator:result-stdout result)))
        (is (equal '("42") (cl-mcp-server.evaluator:result-values result)))))))

(test evaluate-with-stderr
  "Capture error output"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(format *error-output* \"Error message~%\") :ok")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (search "Error message" (cl-mcp-server.evaluator:result-stderr result)))))))

(test evaluate-with-print
  "Capture print output"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(print 'hello) (print 'world)")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (search "HELLO" (cl-mcp-server.evaluator:result-stdout result)))
        (is (search "WORLD" (cl-mcp-server.evaluator:result-stdout result)))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: FAIL - stdout not captured

**Step 3: Implement output capture**

Update `evaluate-code` in `src/evaluator.lisp`:

```lisp
;;; Core evaluation with output capture

(defun evaluate-code (code-string &optional (session cl-mcp-server.session:*session*))
  "Evaluate Common Lisp code in the session context.
   Captures stdout, stderr, and warnings.
   Returns an evaluation-result struct."
  (let* ((pkg (cl-mcp-server.session:session-package session))
         (*package* pkg)
         (stdout-capture (make-string-output-stream))
         (stderr-capture (make-string-output-stream))
         (warnings-list nil))
    ;; Read forms
    (let ((forms (handler-case
                     (read-all-forms code-string)
                   (error (c)
                     (return-from evaluate-code
                       (make-evaluation-result
                         :success-p nil
                         :error-info (list :type (symbol-name (type-of c))
                                           :message (princ-to-string c)
                                           :formatted (cl-mcp-server.error-format:format-error c))))))))
      ;; Evaluate with stream capture
      (let ((result-values nil))
        (handler-case
            (let ((*standard-output* stdout-capture)
                  (*error-output* stderr-capture)
                  (*trace-output* stderr-capture))
              (handler-bind
                  ((warning (lambda (c)
                              (push (cl-mcp-server.error-format:format-warning c)
                                    warnings-list)
                              (muffle-warning c))))
                (dolist (form forms)
                  (setf result-values (multiple-value-list (eval form))))
                (make-evaluation-result
                  :success-p t
                  :values (format-values result-values)
                  :stdout (get-output-stream-string stdout-capture)
                  :stderr (get-output-stream-string stderr-capture)
                  :warnings (nreverse warnings-list))))
          (error (c)
            (make-evaluation-result
              :success-p nil
              :stdout (get-output-stream-string stdout-capture)
              :stderr (get-output-stream-string stderr-capture)
              :warnings (nreverse warnings-list)
              :error-info (list :type (symbol-name (type-of c))
                                :message (princ-to-string c)
                                :formatted (cl-mcp-server.error-format:format-error c)))))))))
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/evaluator.lisp tests/evaluator-tests.lisp
git commit -m "feat: implement output stream capture"
```

---

## Task 4: Implement Warning Capture

**Files:**
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/evaluator-tests.lisp`:

```lisp
(test evaluate-with-warning
  "Capture warnings without stopping execution"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(warn \"Test warning\") 42")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (equal '("42") (cl-mcp-server.evaluator:result-values result)))
        (is (not (null (cl-mcp-server.evaluator:result-warnings result))))
        (is (search "Test warning" (first (cl-mcp-server.evaluator:result-warnings result))))))))

(test evaluate-multiple-warnings
  "Capture multiple warnings"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(warn \"First\") (warn \"Second\") :done")))
        (is (cl-mcp-server.evaluator:result-success-p result))
        (is (= 2 (length (cl-mcp-server.evaluator:result-warnings result))))))))
```

**Step 2: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS (already implemented in previous task)

**Step 3: Commit test additions**

```bash
git add tests/evaluator-tests.lisp
git commit -m "test: add warning capture tests"
```

---

## Task 5: Implement Error Handling

**Files:**
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/evaluator-tests.lisp`:

```lisp
(test evaluate-undefined-function
  "Handle undefined function error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(this-function-does-not-exist-12345)")))
        (is (not (cl-mcp-server.evaluator:result-success-p result)))
        (is (search "UNDEFINED-FUNCTION"
                    (getf (cl-mcp-server.evaluator:result-error result) :type)))))))

(test evaluate-type-error
  "Handle type error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 \"not a number\")")))
        (is (not (cl-mcp-server.evaluator:result-success-p result)))
        (is (search "TYPE-ERROR"
                    (getf (cl-mcp-server.evaluator:result-error result) :type)))))))

(test evaluate-division-by-zero
  "Handle division by zero"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(/ 1 0)")))
        (is (not (cl-mcp-server.evaluator:result-success-p result)))
        (is (search "DIVISION-BY-ZERO"
                    (getf (cl-mcp-server.evaluator:result-error result) :type)))))))

(test evaluate-reader-error
  "Handle reader error (syntax)"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2")))
        (is (not (cl-mcp-server.evaluator:result-success-p result)))
        ;; Reader error or end-of-file
        (is (not (null (cl-mcp-server.evaluator:result-error result))))))))

(test evaluate-error-preserves-output
  "Error preserves output captured before error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.evaluator:evaluate-code
                      "(print 'before-error) (error \"boom\")")))
        (is (not (cl-mcp-server.evaluator:result-success-p result)))
        (is (search "BEFORE-ERROR" (cl-mcp-server.evaluator:result-stdout result)))))))
```

**Step 2: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS (already implemented)

**Step 3: Commit test additions**

```bash
git add tests/evaluator-tests.lisp
git commit -m "test: add error handling tests"
```

---

## Task 6: Implement Result Formatting

**Files:**
- Modify: `src/evaluator.lisp`
- Modify: `tests/evaluator-tests.lisp`

**Step 1: Write the failing test**

Add to `tests/evaluator-tests.lisp`:

```lisp
(test format-result-success
  "Format success result as string"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2)"))
             (formatted (cl-mcp-server.evaluator:format-result result)))
        (is (search "=> 3" formatted))))))

(test format-result-with-output
  "Format result with stdout"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((result (cl-mcp-server.evaluator:evaluate-code
                       "(format t \"hello~%\") 42"))
             (formatted (cl-mcp-server.evaluator:format-result result)))
        (is (search "[stdout]" formatted))
        (is (search "hello" formatted))
        (is (search "=> 42" formatted))))))

(test format-result-error
  "Format error result"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((result (cl-mcp-server.evaluator:evaluate-code "(error \"test\")"))
             (formatted (cl-mcp-server.evaluator:format-result result)))
        (is (search "[ERROR]" formatted))))))
```

**Step 2: Run test to verify it fails**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: FAIL - format-result not implemented

**Step 3: Implement format-result**

Add to `src/evaluator.lisp`:

```lisp
;;; Result formatting for MCP response

(defun format-result (result)
  "Format an evaluation result as a string for MCP response.
   Includes sections for stdout, stderr, warnings, and values/error."
  (with-output-to-string (s)
    ;; Stdout section
    (when (and (result-stdout result)
               (not (zerop (length (result-stdout result)))))
      (format s "[stdout]~%~a~%~%" (result-stdout result)))
    ;; Stderr section
    (when (and (result-stderr result)
               (not (zerop (length (result-stderr result)))))
      (format s "[stderr]~%~a~%~%" (result-stderr result)))
    ;; Warnings section
    (when (result-warnings result)
      (format s "[warnings]~%")
      (dolist (w (result-warnings result))
        (format s "~a~%" w))
      (terpri s))
    ;; Result or error
    (if (result-success-p result)
        ;; Values
        (dolist (v (result-values result))
          (format s "=> ~a~%" v))
        ;; Error
        (write-string (getf (result-error result) :formatted) s))))
```

Update package exports:
```lisp
#:format-result
```

**Step 4: Run test to verify it passes**

Run: `sbcl --noinform --non-interactive --eval "(ql:quickload :cl-mcp-server/tests)" --eval "(fiveam:run! 'cl-mcp-server-tests::evaluator-tests)"`
Expected: PASS

**Step 5: Commit**

```bash
git add src/evaluator.lisp src/packages.lisp tests/evaluator-tests.lisp
git commit -m "feat: implement result formatting for MCP"
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

Test evaluator interactively:
```lisp
(ql:quickload :cl-mcp-server)
(let ((s (cl-mcp-server.session:make-session)))
  (cl-mcp-server.session:with-session (s)
    (print (cl-mcp-server.evaluator:format-result
             (cl-mcp-server.evaluator:evaluate-code
               "(format t \"Hello~%\") (warn \"Caution\") (+ 1 2 3)")))))
```
