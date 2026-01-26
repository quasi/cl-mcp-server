---
type: verification
name: state-persistence-property-test
source: properties/state-persistence.md
level: property
tags:
  - property-based
  - session
---

# Property Test: State Persistence

## Purpose

Verify that all definitions persist across evaluations using property-based testing.

## Prerequisites

- Initialized server
- Property-based testing framework (FiveAM with generators)

## Implementation

### Generator: Random Definitions

```lisp
(defun generate-random-definition ()
  "Generate random but valid definition forms"
  (let ((name (format nil "TEST-~A-~A" (random-choice '(FN VAR MAC)) (random 10000))))
    (random-choice
     (list
      ;; Function
      `(defun ,(intern name) (x) (* x ,(1+ (random 10))))
      ;; Variable
      `(defvar ,(intern (format nil "*~A*" name)) ,(random 1000))
      ;; Parameter
      `(defparameter ,(intern (format nil "*~A*" name)) ,(random 1000))
      ;; Macro
      `(defmacro ,(intern name) (x) `(+ ,x 1))))))

(defun definition-symbol (def-form)
  "Extract the symbol being defined"
  (second def-form))

(defun definition-type (def-form)
  "Determine definition type"
  (first def-form))
```

### Property: Definitions Persist

```lisp
(test state-persistence-property
  "All successfully evaluated definitions must persist"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 100)
           (let* ((def-form (generate-random-definition))
                  (symbol (definition-symbol def-form))
                  (def-type (definition-type def-form))
                  (code (format nil "~S" def-form)))

             ;; Define
             (let ((r1 (send-request server
                         `((:jsonrpc . "2.0")
                           (:id . ,(* trial 2))
                           (:method . "tools/call")
                           (:params . ((:name . "evaluate-lisp")
                                      (:arguments . ((:code . ,code)))))))))

               (when (result-response-p r1)
                 ;; If definition succeeded, verify it persists
                 (let* ((check-code
                         (case def-type
                           ((defun defmacro)
                            (format nil "(fboundp '~A)" symbol))
                           ((defvar defparameter)
                            (format nil "(boundp '~A)" symbol))
                           (t "(error \"Unknown type\")")))
                        (r2 (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,(1+ (* trial 2)))
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,check-code)))))))))

                   (is (result-response-p r2)
                       "Check request should succeed: ~A" check-code)
                   (is (search "T" (extract-result-text r2) :test #'char-equal)
                       "Symbol ~A should be ~A after definition"
                       symbol
                       (case def-type
                         ((defun defmacro) "fbound")
                         (t "bound"))))))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Definition Accumulation

```lisp
(test definition-accumulation-property
  "Definitions accumulate over multiple evaluations"
  (let ((server (make-initialized-test-server))
        (defined-symbols nil))
    (unwind-protect
         (progn
           ;; Create N definitions
           (dotimes (i 20)
             (let* ((name (format nil "ACCUM-FN-~A" i))
                    (code (format nil "(defun ~A (x) (+ x ~A))" name i)))
               (let ((response (send-request server
                                 `((:jsonrpc . "2.0")
                                   (:id . ,i)
                                   (:method . "tools/call")
                                   (:params . ((:name . "evaluate-lisp")
                                              (:arguments . ((:code . ,code)))))))))
                 (when (result-response-p response)
                   (push (intern name) defined-symbols)))))

           ;; All should still be defined
           (dolist (sym defined-symbols)
             (let* ((code (format nil "(fboundp '~A)" sym))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . 1000)
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,code)))))))))
               (is (result-response-p response))
               (is (search "T" (extract-result-text response) :test #'char-equal)
                   "Symbol ~A should still be fbound" sym))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Interdependent Definitions

```lisp
(test interdependent-definitions-property
  "Functions can be defined in terms of previously defined functions"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 50)
           ;; Define base function
           (let* ((base-name (format nil "BASE-~A" trial))
                  (base-code (format nil "(defun ~A (x) (* x 2))" base-name)))
             (let ((r1 (send-request server
                         `((:jsonrpc . "2.0")
                           (:id . ,(* trial 3))
                           (:method . "tools/call")
                           (:params . ((:name . "evaluate-lisp")
                                      (:arguments . ((:code . ,base-code)))))))))
               (is (result-response-p r1)))

             ;; Define dependent function
             (let* ((dep-name (format nil "DEP-~A" trial))
                    (dep-code (format nil "(defun ~A (x) (~A (+ x 1)))" dep-name base-name)))
               (let ((r2 (send-request server
                           `((:jsonrpc . "2.0")
                             (:id . ,(1+ (* trial 3)))
                             (:method . "tools/call")
                             (:params . ((:name . "evaluate-lisp")
                                        (:arguments . ((:code . ,dep-code)))))))))
                 (is (result-response-p r2)))

               ;; Use dependent function
               (let* ((use-code (format nil "(~A 5)" dep-name))
                      (r3 (send-request server
                            `((:jsonrpc . "2.0")
                              (:id . ,(+ 2 (* trial 3)))
                              (:method . "tools/call")
                              (:params . ((:name . "evaluate-lisp")
                                         (:arguments . ((:code . ,use-code)))))))))
                 (is (result-response-p r3))
                 (is (search "12" (extract-result-text r3))
                     "Result should be 12: (5+1)*2")))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Variable Accumulation

```lisp
(test variable-state-accumulation-property
  "Variable values accumulate across evaluations"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (progn
           ;; Initialize counter
           (let ((r1 (send-request server
                       '((:jsonrpc . "2.0")
                         (:id . 1)
                         (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp")
                                    (:arguments . ((:code . "(defvar *property-counter* 0)")))))))))
             (is (result-response-p r1)))

           ;; Increment N times
           (dotimes (i 50)
             (let ((response (send-request server
                               '((:jsonrpc . "2.0")
                                 (:id . 2)
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . "(incf *property-counter*)")))))))))
               (is (result-response-p response))))

           ;; Final value should be 50
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 3)
                               (:method . "tools/call")
                               (:params . ((:name . "evaluate-lisp")
                                          (:arguments . ((:code . "*property-counter*")))))))))
             (is (result-response-p response))
             (is (search "50" (extract-result-text response))
                 "Counter should accumulate to 50")))
      (cl-mcp-server:stop-test-server server))))
```

## Configuration

- Trials: 100 for normal runs
- Trials: 1000 for thorough testing

## Assertions

For every generated definition:
1. If definition succeeds, symbol must be fbound/bound in next evaluation
2. Multiple definitions must accumulate (not replace all state)
3. Definitions can reference previously defined symbols
4. Variable values must accumulate across evaluations

## Notes

- Focuses on the core persistence property
- Uses random generation to test edge cases
- Verifies both definition existence and usability
- Tests interdependencies between definitions
