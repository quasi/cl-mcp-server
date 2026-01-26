---
type: verification
name: definitions-scenario-test
source: scenarios/definitions.md
level: scenario
tags:
  - integration
  - state-persistence
---

# Scenario Test: Function and Variable Definitions

## Purpose

Verify that functions, variables, macros, and classes defined in one evaluation persist and are usable in subsequent evaluations.

## Prerequisites

- Fresh server instance or reset session
- MCP initialized

## Setup

```lisp
(defvar *test-server* (cl-mcp-server:start-test-server))

(defun eval-code (id code)
  (send-request *test-server*
               `((:jsonrpc . "2.0")
                 (:id . ,id)
                 (:method . "tools/call")
                 (:params . ((:name . "evaluate-lisp")
                            (:arguments . ((:code . ,code))))))))
```

## Execution

### Scenario 1: Function Definition and Usage

#### Step 1.1: Define Function

```lisp
(let* ((response (eval-code 1 "(defun square (x) (* x x))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (not (result-field response :isError)))
  (assert (search "SQUARE" text)))
```

#### Step 1.2: Use Function

```lisp
(let* ((response (eval-code 2 "(square 5)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 25" text)))
```

#### Step 1.3: Use Function in Complex Expression

```lisp
(let* ((response (eval-code 3 "(mapcar #'square '(1 2 3 4))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "(1 4 9 16)" text)))
```

### Scenario 2: Variable Definition and Modification

#### Step 2.1: Define Variable

```lisp
(let* ((response (eval-code 4 "(defvar *counter* 0)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "*COUNTER*" text)))
```

#### Step 2.2: Modify and Read Variable

```lisp
(let* ((response (eval-code 5 "(incf *counter*) (incf *counter*) *counter*"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 2" text)))
```

#### Step 2.3: Verify Modification Persisted

```lisp
(let* ((response (eval-code 6 "*counter*"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 2" text)))
```

### Scenario 3: Macro Definition and Usage

#### Step 3.1: Define Macro

```lisp
(let* ((macro-def "(defmacro twice (x) `(+ ,x ,x))")
       (response (eval-code 7 macro-def))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "TWICE" text)))
```

#### Step 3.2: Use Macro

```lisp
(let* ((response (eval-code 8 "(twice 21)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 42" text)))
```

#### Step 3.3: Use Macro in Nested Context

```lisp
(let* ((response (eval-code 9 "(twice (square 3))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "=> 18" text)))  ; (+ 9 9)
```

### Scenario 4: Class Definition and Usage

#### Step 4.1: Define Class

```lisp
(let* ((class-def "(defclass point () ((x :initarg :x :accessor point-x) (y :initarg :y :accessor point-y)))")
       (response (eval-code 10 class-def))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "POINT" text)))
```

#### Step 4.2: Create Instance

```lisp
(let* ((response (eval-code 11 "(defvar *p* (make-instance 'point :x 3 :y 4))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "*P*" text)))
```

#### Step 4.3: Access Instance Slots

```lisp
(let* ((response (eval-code 12 "(list (point-x *p*) (point-y *p*))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "(3 4)" text)))
```

#### Step 4.4: Compute with Instance Data

```lisp
(let* ((response (eval-code 13 "(sqrt (+ (expt (point-x *p*) 2) (expt (point-y *p*) 2)))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "5" text)))  ; 3-4-5 triangle
```

## Verification

### All Definitions Accessible

```lisp
;; Test that all defined names are still accessible
(let* ((check-code "(list (fboundp 'square) (boundp '*counter*) (macro-function 'twice) (find-class 'point nil))")
       (response (eval-code 100 check-code))
       (text (result-content-text response)))
  (assert (result-response-p response))
  ;; All should be true/non-nil
  (assert (not (search "NIL NIL" text))))
```

### Function Definitions Are Callable

```lisp
(let* ((response (eval-code 101 "(square 10)"))
       (text (result-content-text response)))
  (assert (search "=> 100" text)))
```

### Variable State Persisted

```lisp
(let* ((response (eval-code 102 "*counter*"))
       (text (result-content-text response)))
  (assert (search "=> 2" text)))
```

### Macro Expansion Works

```lisp
(let* ((response (eval-code 103 "(macroexpand-1 '(twice 5))"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "(+ 5 5)" text)))
```

### Class Instances Persist

```lisp
(let* ((response (eval-code 104 "(class-of *p*)"))
       (text (result-content-text response)))
  (assert (result-response-p response))
  (assert (search "POINT" text)))
```

### Complex Interactions Work

```lisp
;; Use all definitions together
(let* ((code "(incf *counter* (twice (square 2)))")
       (response (eval-code 105 code))
       (text (result-content-text response)))
  (assert (result-response-p response))
  ;; *counter* was 2, square(2) = 4, twice(4) = 8, so 2 + 8 = 10
  (assert (search "=> 10" text)))
```

### Redefining Works

```lisp
;; Redefine square function
(let* ((response (eval-code 106 "(defun square (x) (+ x x))"))  ; Now adds instead of multiplies
       (text (result-content-text response)))
  (assert (result-response-p response)))

;; New definition should be active
(let* ((response (eval-code 107 "(square 5)"))
       (text (result-content-text response)))
  (assert (search "=> 10" text)))  ; 5 + 5, not 25
```

## Teardown

```lisp
(cl-mcp-server:stop-test-server *test-server*)
```

## Notes

- This scenario verifies INV-003 (Session State Persistence)
- All Common Lisp definition forms persist: defun, defvar, defmacro, defclass
- State modifications (incf, setf) persist across evaluations
- Definitions can be redefined and new definition takes effect
- All definitions remain available throughout session lifetime
