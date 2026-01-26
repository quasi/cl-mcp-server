---
type: verification
name: state-persistence-property-test
source: properties/state-persistence.md
level: property
tags:
  - property-based
  - state
  - persistence
---

# Property Test: State Persistence

## Purpose

Verify that all session state (functions, variables, macros, classes, package changes) persists across evaluations within a session.

## Prerequisites

- Property-based testing framework
- Test session with clean state

## Implementation

### Generator: Random State-Modifying Operations

```lisp
(defun generate-state-operation ()
  "Generate code that creates or modifies state"
  (let ((name (generate-random-symbol)))
    (random-choice
     (list
      ;; Function definition
      (cons :function
            (format nil "(defun ~A (x) (+ x 1))" name))

      ;; Variable definition
      (cons :variable
            (format nil "(defvar ~A ~A)" name (random 1000)))

      ;; Variable modification
      (cons :setf
            (format nil "(setf ~A ~A)" name (random 1000)))

      ;; Macro definition
      (cons :macro
            (format nil "(defmacro ~A (x) `(* ,x 2))" name))

      ;; Class definition
      (cons :class
            (format nil "(defclass ~A () ((slot :initarg :slot)))" name))))))

(defun generate-random-symbol ()
  (intern (format nil "TEST-~A" (random 10000)) :cl-user))

(defun random-choice (list)
  (nth (random (length list)) list))
```

### Property 1: Definitions Persist

```lisp
(fiveam:test definitions-persist
  "Defined functions, variables, macros, classes persist"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((op (generate-state-operation))
             (type (car op))
             (code (cdr op))
             (name (extract-symbol-from-code code)))

        ;; Define something
        (let ((r1 (evaluate-in-session session code)))
          (fiveam:is (not (result-field r1 :isError))))

        ;; Verify it persists
        (let* ((check-code
                (ecase type
                  (:function (format nil "(fboundp '~A)" name))
                  (:variable (format nil "(boundp '~A)" name))
                  (:setf (format nil "(boundp '~A)" name))
                  (:macro (format nil "(macro-function '~A)" name))
                  (:class (format nil "(find-class '~A nil)" name))))
               (r2 (evaluate-in-session session check-code))
               (text (result-value r2)))

          ;; Should return non-NIL
          (fiveam:is (not (string-equal "NIL" (string-trim '(#\Space #\Newline) text))))
          (fiveam:is (not (search "=> NIL" text))))))))
```

### Property 2: Function Calls Work After Definition

```lisp
(fiveam:test functions-callable-after-definition
  "Functions can be called in subsequent evaluations"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((func-name (generate-random-symbol))
             (def-code (format nil "(defun ~A (x) (* x 2))" func-name))
             (test-value (+ 1 (random 100))))

        ;; Define function
        (let ((r1 (evaluate-in-session session def-code)))
          (fiveam:is (not (result-field r1 :isError))))

        ;; Call function
        (let* ((call-code (format nil "(~A ~A)" func-name test-value))
               (r2 (evaluate-in-session session call-code))
               (text (result-value r2))
               (expected (* test-value 2)))
          (fiveam:is (not (result-field r2 :isError)))
          (fiveam:is (search (prin1-to-string expected) text)))))))
```

### Property 3: Variable Modifications Persist

```lisp
(fiveam:test variable-modifications-persist
  "Variable changes are visible in subsequent evaluations"
  (let ((session (make-test-session)))
    (dotimes (trial 50)
      (let* ((var-name (generate-random-symbol))
             (initial (random 100))
             (increment (+ 1 (random 50))))

        ;; Define variable
        (evaluate-in-session session (format nil "(defvar ~A ~A)" var-name initial))

        ;; Modify it
        (evaluate-in-session session (format nil "(incf ~A ~A)" var-name increment))

        ;; Check value
        (let* ((r (evaluate-in-session session (prin1-to-string var-name)))
               (text (result-value r))
               (expected (+ initial increment)))
          (fiveam:is (search (prin1-to-string expected) text)))))))
```

### Property 4: Multiple Definitions Coexist

```lisp
(fiveam:test multiple-definitions-coexist
  "Multiple definitions in same session all persist"
  (let ((session (make-test-session))
        (definitions '()))

    ;; Create multiple definitions
    (dotimes (i 10)
      (let* ((name (generate-random-symbol))
             (value (random 1000))
             (code (format nil "(defvar ~A ~A)" name value)))
        (push (cons name value) definitions)
        (evaluate-in-session session code)))

    ;; Verify all still exist
    (dolist (def definitions)
      (let* ((name (car def))
             (expected (cdr def))
             (r (evaluate-in-session session (prin1-to-string name)))
             (text (result-value r)))
        (fiveam:is (search (prin1-to-string expected) text)
                  "Lost definition for ~A" name)))))
```

### Property 5: Package Changes Persist

```lisp
(fiveam:test package-changes-persist
  "Package context changes persist across evaluations"
  (let ((session (make-test-session)))
    ;; Change to KEYWORD package
    (evaluate-in-session session "(in-package :keyword)")

    ;; Next evaluation should be in KEYWORD
    (let* ((r (evaluate-in-session session "(package-name *package*)"))
           (text (result-value r)))
      (fiveam:is (search "KEYWORD" text)))

    ;; Return to CL-USER for cleanup
    (evaluate-in-session session "(in-package :cl-user)")))
```

### Property 6: Long Session Persistence

```lisp
(fiveam:test long-session-persistence
  "State persists across many evaluations"
  (let ((session (make-test-session))
        (var-name (generate-random-symbol)))

    ;; Define variable at start
    (evaluate-in-session session (format nil "(defvar ~A 0)" var-name))

    ;; Do 100 other operations
    (dotimes (i 100)
      (evaluate-in-session session "(+ 1 2)"))

    ;; Variable should still exist
    (let* ((r (evaluate-in-session session (prin1-to-string var-name)))
           (text (result-value r)))
      (fiveam:is (search "0" text)))))
```

### Property 7: Redefining Works

```lisp
(fiveam:test redefining-works
  "Can redefine functions and see new behavior"
  (let ((session (make-test-session))
        (func-name (generate-random-symbol)))

    ;; Define function version 1
    (evaluate-in-session session
                        (format nil "(defun ~A (x) (* x 2))" func-name))

    ;; Test version 1
    (let* ((r1 (evaluate-in-session session (format nil "(~A 5)" func-name)))
           (text1 (result-value r1)))
      (fiveam:is (search "10" text1)))

    ;; Redefine function version 2
    (evaluate-in-session session
                        (format nil "(defun ~A (x) (+ x 100))" func-name))

    ;; Test version 2 - should see new behavior
    (let* ((r2 (evaluate-in-session session (format nil "(~A 5)" func-name)))
           (text2 (result-value r2)))
      (fiveam:is (search "105" text2))
      (fiveam:is (not (search "10" text2))))))
```

### Property 8: Complex State Interactions

```lisp
(fiveam:test complex-state-interactions
  "Multiple state elements can interact"
  (let ((session (make-test-session)))
    ;; Define function
    (evaluate-in-session session "(defun double (x) (* x 2))")

    ;; Define variable
    (evaluate-in-session session "(defvar *value* 10)")

    ;; Define macro using function and variable
    (evaluate-in-session session
                        "(defmacro compute () `(double *value*))")

    ;; Use macro
    (let* ((r (evaluate-in-session session "(compute)"))
           (text (result-value r)))
      (fiveam:is (search "20" text)))

    ;; Modify variable
    (evaluate-in-session session "(setf *value* 15)")

    ;; Use macro again - should see new value
    (let* ((r (evaluate-in-session session "(compute)"))
           (text (result-value r)))
      (fiveam:is (search "30" text)))))
```

## Configuration

- Trials: 50 for normal runs
- Trials: 200 for thorough testing
- Shrinking: Enabled

## Helper Functions

```lisp
(defun extract-symbol-from-code (code)
  "Extract the symbol being defined from code"
  (let* ((tokens (split-string code))
         (name-token (third tokens)))
    (intern (string-trim "()" name-token) :cl-user)))

(defun split-string (string)
  "Simple string splitter"
  (loop with start = 0
        for end = (position #\Space string :start start)
        collect (subseq string start end)
        while end
        do (setf start (1+ end))))
```

## Assertions

For every trial:
1. Defined elements persist across evaluations
2. Functions remain callable
3. Variable modifications are visible
4. Multiple definitions coexist
5. Package changes persist
6. State survives long sessions
7. Redefinitions take effect
8. Complex state interactions work

## Notes

- This test verifies INV-003 (Session State Persistence)
- Tests all state types: functions, variables, macros, classes
- Verifies both creation and modification
- Tests interaction between state elements
- Confirms state survives many evaluations
