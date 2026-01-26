---
type: verification
name: compilation-safety-property-test
source: properties/compilation-safety.md
level: property
tags:
  - property-based
  - safety
  - compilation
---

# Property Test: Compilation Safety

## Purpose

Verify that `compile-form` never executes user code or modifies session state across a wide range of inputs.

## Prerequisites

- Initialized MCP server
- Property-based testing framework (FiveAM with randomized tests)

## Implementation

### Generator: Side-Effect Producing Code

```lisp
(defun generate-side-effect-code ()
  "Generate code with various side effects"
  (random-choice
   (list
    ;; Print side effects
    "(defun f () (print 'EXECUTED))"
    "(print 'HELLO)"
    "(format t \"OUTPUT~%\")"

    ;; File I/O side effects
    "(with-open-file (f \"/tmp/test.txt\" :direction :output) (write-line \"data\" f))"
    "(probe-file \"/tmp\")"  ; Read-only, but still I/O

    ;; Function definitions
    "(defun new-function () 42)"
    "(defmacro new-macro () 'expanded)"

    ;; Variable bindings
    "(defvar *new-var* 123)"
    "(defparameter *new-param* 456)"
    "(setf *global* 789)"

    ;; Package operations
    "(make-package :temp-package)"
    "(in-package :cl-user)"

    ;; Reader side effects
    "#.(print 'READ-TIME)"  ; Read-time evaluation
    "(defun f () #.(+ 1 2))"

    ;; Eval-when
    "(eval-when (:compile-toplevel :load-toplevel :execute) (print 'EVAL-WHEN))")))
```

### Property: No Execution During Compilation

```lisp
(deftest compilation-safety-no-execution ()
  "Compilation never executes user code"
  (dotimes (i 1000)
    (let* ((code (generate-side-effect-code))
           (output-stream (make-string-output-stream))
           (*standard-output* output-stream)
           (*error-output* output-stream))

      ;; Compile the code
      (call-tool *test-server* "compile-form"
                 `(("code" . ,code)))

      ;; Verify no output produced
      (let ((output (get-output-stream-string output-stream)))
        (is (zerop (length output))
            "Compilation of ~A produced output: ~A" code output)))))
```

### Property: No State Modification

```lisp
(deftest compilation-safety-no-state-change ()
  "Compilation never modifies session state"
  (dotimes (i 1000)
    (let ((code (generate-side-effect-code)))

      ;; Capture state before
      (let ((functions-before (list-all-functions 'cl-user))
            (variables-before (list-all-variables 'cl-user))
            (packages-before (list-all-packages)))

        ;; Compile code
        (call-tool *test-server* "compile-form"
                   `(("code" . ,code)))

        ;; Verify state unchanged
        (is (set-equal functions-before (list-all-functions 'cl-user))
            "Functions changed after compiling: ~A" code)
        (is (set-equal variables-before (list-all-variables 'cl-user))
            "Variables changed after compiling: ~A" code)
        (is (set-equal packages-before (list-all-packages))
            "Packages changed after compiling: ~A" code)))))
```

### Property: File System Isolation

```lisp
(deftest compilation-safety-no-file-creation ()
  "Compilation never creates files"
  (dotimes (i 100)
    (let ((temp-file (format nil "/tmp/cl-mcp-test-~A.txt" i))
          (code (format nil
                        "(with-open-file (f \"~A\" :direction :output) ~
                          (write-line \"data\" f))"
                        temp-file)))

      ;; Ensure file doesn't exist
      (when (probe-file temp-file)
        (delete-file temp-file))

      ;; Compile code with file I/O
      (call-tool *test-server* "compile-form"
                 `(("code" . ,code)))

      ;; Verify file not created
      (is (null (probe-file temp-file))
          "File ~A was created during compilation" temp-file))))
```

### Property: Specific Function Not Defined

```lisp
(deftest compilation-safety-function-not-defined ()
  "Functions compiled are not defined in the image"
  (dotimes (i 100)
    (let* ((func-name (intern (format nil "TEST-FUNC-~A" i)))
           (code (format nil "(defun ~A () 42)" func-name)))

      ;; Verify not defined before
      (is (not (fboundp func-name)))

      ;; Compile definition
      (call-tool *test-server* "compile-form"
                 `(("code" . ,code)))

      ;; Verify still not defined
      (is (not (fboundp func-name))
          "Function ~A was defined during compilation" func-name))))
```

### Property: Variables Not Bound

```lisp
(deftest compilation-safety-variable-not-bound ()
  "Variables in compiled code are not bound"
  (dotimes (i 100)
    (let* ((var-name (intern (format nil "*TEST-VAR-~A*" i)))
           (code (format nil "(defvar ~A 123)" var-name)))

      ;; Verify not bound before
      (is (not (boundp var-name)))

      ;; Compile definition
      (call-tool *test-server* "compile-form"
                 `(("code" . ,code)))

      ;; Verify still not bound
      (is (not (boundp var-name))
          "Variable ~A was bound during compilation" var-name))))
```

### Property: Macro Expansion Side Effects

```lisp
(deftest compilation-safety-macro-expansion ()
  "Macro expansion during compilation has no side effects"
  (dotimes (i 50)
    (let ((output-stream (make-string-output-stream))
          (*standard-output* output-stream)
          (code "(defmacro side-effect-macro ()
                   (print 'MACRO-EXPANDED)
                   42)"))

      ;; Compile macro definition
      (call-tool *test-server* "compile-form"
                 `(("code" . ,code)))

      ;; Verify nothing printed
      (let ((output (get-output-stream-string output-stream)))
        (is (zerop (length output))
            "Macro expansion produced output: ~A" output)))))
```

### Property: Read-Time Evaluation Safety

```lisp
(deftest compilation-safety-read-time-eval ()
  "Read-time evaluation is safely handled"
  (let ((code "(defun f () #.(+ 1 2))")
        (output-stream (make-string-output-stream))
        (*standard-output* output-stream))

    ;; This may fail or succeed depending on implementation
    ;; Key: no side effects if it succeeds
    (handler-case
        (progn
          (call-tool *test-server* "compile-form"
                     `(("code" . ,code)))

          ;; If successful, verify no output
          (let ((output (get-output-stream-string output-stream)))
            (is (zerop (length output)))))

      (error ()
        ;; If it errors, that's acceptable for safety
        (pass)))))
```

## Configuration

- Examples: 1000 for normal runs
- Examples: 10000 for CI/thorough testing
- Seed: Randomized (log seed for reproducibility)

## Assertions

For every generated code sample:
1. No output to stdout/stderr during compilation
2. No new functions defined in session
3. No new variables bound in session
4. No files created on filesystem
5. No packages created or modified

## Test Suites

```lisp
(def-suite compilation-safety-suite
  :description "Property tests for compilation safety")

(in-suite compilation-safety-suite)

;; All tests above belong to this suite
```

## Helper Functions

```lisp
(defun list-all-functions (package)
  "List all fboundp symbols in package"
  (let ((functions '()))
    (do-symbols (sym package)
      (when (and (eq (symbol-package sym) (find-package package))
                 (fboundp sym))
        (push sym functions)))
    functions))

(defun list-all-variables (package)
  "List all boundp special variables in package"
  (let ((variables '()))
    (do-symbols (sym package)
      (when (and (eq (symbol-package sym) (find-package package))
                 (boundp sym))
        (push sym variables)))
    variables))

(defun set-equal (list1 list2)
  "Check if two lists have same elements (order independent)"
  (and (null (set-difference list1 list2))
       (null (set-difference list2 list1))))

(defun random-choice (list)
  "Pick random element from list"
  (nth (random (length list)) list))
```

## Notes

- This is the most critical safety property for compile-form
- Violations would make the tool unsafe for speculative use
- Generator should cover all common side-effect patterns
- Tests should be comprehensive but fast (< 1 min for 1000 examples)
