---
type: verification
name: evaluation-isolation-property-test
source: properties/evaluation-isolation.md
level: property
tags:
  - property-based
  - isolation
---

# Property Test: Evaluation Isolation

## Purpose

Verify that session isolation is maintained: definitions persist within sessions but don't leak between sessions, and user code cannot corrupt server state.

## Prerequisites

- Property-based testing framework (FiveAM with randomized tests)
- Ability to create multiple isolated test sessions

## Implementation

### Generator: Random Definition Forms

```lisp
(defun generate-random-definition ()
  "Generate random Common Lisp definition forms"
  (let ((name (generate-random-symbol)))
    (random-choice
     (list
      ;; Function definition
      `(defun ,name (x) (* x 2))

      ;; Variable definition
      `(defvar ,name 42)

      ;; Macro definition
      `(defmacro ,name (x) `(+ ,x ,x))

      ;; Class definition
      `(defclass ,name () ((slot :initarg :slot)))))))

(defun generate-random-symbol ()
  "Generate random symbol name"
  (intern (format nil "TEST-~A-~A"
                  (random-choice '("FOO" "BAR" "BAZ" "QUX"))
                  (random 1000))
          :cl-user))

(defun random-choice (list)
  (nth (random (length list)) list))
```

### Property 1: Within-Session Persistence

**Property**: Definitions remain available in subsequent evaluations within the same session.

```lisp
(fiveam:test within-session-persistence
  "Definitions persist within a session"
  (dotimes (trial 100)
    (let ((session (make-test-session))
          (def-form (generate-random-definition)))

      ;; Define something
      (let ((r1 (evaluate-in-session session (prin1-to-string def-form))))
        (fiveam:is (result-success-p r1)))

      ;; Extract the name that was defined
      (let* ((name (extract-defined-name def-form))
             (check-form (format nil "(or (fboundp '~A) (boundp '~A) (macro-function '~A) (find-class '~A nil))"
                                name name name name))
             (r2 (evaluate-in-session session check-form)))
        ;; Should still be accessible
        (fiveam:is (result-success-p r2))
        (fiveam:is (not (search "NIL" (result-value r2))))))))
```

### Property 2: Cross-Session Isolation

**Property**: Definitions in one session do not appear in other sessions.

```lisp
(fiveam:test cross-session-isolation
  "Definitions don't leak between sessions"
  (dotimes (trial 100)
    (let ((session-a (make-test-session))
          (session-b (make-test-session))
          (def-form (generate-random-definition)))

      ;; Define in session A
      (let ((r1 (evaluate-in-session session-a (prin1-to-string def-form))))
        (fiveam:is (result-success-p r1)))

      ;; Check it exists in session A
      (let* ((name (extract-defined-name def-form))
             (check-form (format nil "(boundp '~A)" name))
             (r2 (evaluate-in-session session-a check-form)))
        (fiveam:is (result-success-p r2)))

      ;; Check it does NOT exist in session B
      (let* ((name (extract-defined-name def-form))
             (check-form (format nil "(or (fboundp '~A) (boundp '~A))" name name))
             (r3 (evaluate-in-session session-b check-form)))
        (fiveam:is (result-success-p r3))
        ;; Should return NIL (not found)
        (fiveam:is (search "NIL" (result-value r3)))))))
```

### Property 3: Server State Protection

**Property**: User code cannot corrupt MCP server internal functions.

```lisp
(fiveam:test server-state-protection
  "User code cannot redefine server functions"
  (let ((session (make-test-session))
        (server-functions '("HANDLE-INITIALIZE" "HANDLE-TOOLS-LIST"
                           "HANDLE-TOOLS-CALL" "EVALUATE-LISP")))

    (dolist (func-name server-functions)
      ;; Try to redefine server function
      (let* ((evil-code (format nil "(defun ~A (&rest args) :hacked)" func-name))
             (r1 (evaluate-in-session session evil-code)))
        ;; Evaluation might succeed (creates function in user package)
        ;; but shouldn't affect server

        ;; Server should still work - test with actual request
        (let ((response (send-initialize-request)))
          (fiveam:is (result-response-p response))
          (fiveam:is (not (equal :hacked (result-field response :protocolVersion)))))))))
```

### Property 4: Function Definition Persistence

**Property**: Functions can be defined and called in subsequent evaluations.

```lisp
(fiveam:test function-definition-persistence
  "Functions defined in one evaluation are callable in next"
  (dotimes (trial 50)
    (let ((session (make-test-session))
          (func-name (generate-random-symbol))
          (test-value (random 100)))

      ;; Define function
      (let* ((def-code (format nil "(defun ~A (x) (* x 2))" func-name))
             (r1 (evaluate-in-session session def-code)))
        (fiveam:is (result-success-p r1)))

      ;; Call function
      (let* ((call-code (format nil "(~A ~A)" func-name test-value))
             (r2 (evaluate-in-session session call-code))
             (expected (* test-value 2)))
        (fiveam:is (result-success-p r2))
        (fiveam:is (search (prin1-to-string expected) (result-value r2)))))))
```

### Property 5: Variable State Persistence

**Property**: Variables can be defined, modified, and read across evaluations.

```lisp
(fiveam:test variable-state-persistence
  "Variables persist and modifications are visible"
  (dotimes (trial 50)
    (let ((session (make-test-session))
          (var-name (generate-random-symbol))
          (initial-value (random 100)))

      ;; Define variable
      (let* ((def-code (format nil "(defvar ~A ~A)" var-name initial-value))
             (r1 (evaluate-in-session session def-code)))
        (fiveam:is (result-success-p r1)))

      ;; Read variable
      (let* ((read-code (prin1-to-string var-name))
             (r2 (evaluate-in-session session read-code)))
        (fiveam:is (result-success-p r2))
        (fiveam:is (search (prin1-to-string initial-value) (result-value r2))))

      ;; Modify variable
      (let* ((new-value (+ initial-value 10))
             (modify-code (format nil "(setf ~A ~A)" var-name new-value))
             (r3 (evaluate-in-session session modify-code)))
        (fiveam:is (result-success-p r3)))

      ;; Read modified value
      (let* ((read-code (prin1-to-string var-name))
             (r4 (evaluate-in-session session read-code)))
        (fiveam:is (result-success-p r4))
        (fiveam:is (search (prin1-to-string (+ initial-value 10)) (result-value r4)))))))
```

### Property 6: Multiple Sessions Simultaneously

**Property**: Multiple sessions can coexist with independent state.

```lisp
(fiveam:test multiple-sessions-independent
  "Multiple sessions maintain independent state"
  (let ((sessions (loop repeat 5 collect (make-test-session)))
        (var-name '*TEST-VAR*))

    ;; Set different values in each session
    (loop for session in sessions
          for value from 1
          do (let* ((code (format nil "(defvar ~A ~A)" var-name value))
                   (r (evaluate-in-session session code)))
               (fiveam:is (result-success-p r))))

    ;; Verify each session has its own value
    (loop for session in sessions
          for expected from 1
          do (let* ((code (prin1-to-string var-name))
                   (r (evaluate-in-session session code)))
               (fiveam:is (result-success-p r))
               (fiveam:is (search (prin1-to-string expected) (result-value r)))))))
```

## Configuration

- Trials: 100 for normal runs
- Trials: 1000 for CI/thorough testing
- Shrinking: Enabled (find minimal violation)

## Helper Functions

```lisp
(defun extract-defined-name (def-form)
  "Extract the symbol name from a definition form"
  (cadr def-form))

(defun result-success-p (result)
  "Check if result indicates success"
  (not (result-field result :isError)))

(defun result-value (result)
  "Extract value from result"
  (result-content-text result))
```

## Assertions

For every trial:
1. Definitions in a session remain accessible in subsequent evaluations
2. Definitions in one session do not appear in other sessions
3. Server functions remain unaffected by user code
4. Multiple sessions can coexist with independent state

## Notes

- This test verifies INV-003 (Session State Persistence)
- Uses property-based testing to generate random definitions
- Tests both positive (persistence) and negative (isolation) properties
- Verifies server protection from user code interference
