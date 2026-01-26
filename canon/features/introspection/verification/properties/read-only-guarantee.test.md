---
type: verification
name: read-only-guarantee-property-test
source: properties/read-only-guarantee.md
level: property
tags:
  - property-based
  - introspection
  - safety
---

# Property Test: Read-Only Guarantee

## Purpose

Verify that all introspection tools are read-only and don't modify session state.

## Prerequisites

- Initialized server
- Property-based testing framework

## Implementation

### Property: Introspection Doesn't Modify State

```lisp
(defvar *introspection-tools*
  '("describe-symbol" "apropos-search" "who-calls" "who-references"
    "macroexpand-form" "validate-syntax"))

(test introspection-read-only-property
  "Introspection tools must not modify session state"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 50)
           ;; Define some state
           (let* ((fn-name (format nil "STATE-FN-~A" trial))
                  (var-name (format nil "*STATE-VAR-~A*" trial))
                  (def-code (format nil "(defun ~A () ~A) (defvar ~A ~A)"
                                   fn-name trial var-name trial)))
             (let ((r1 (send-request server
                         `((:jsonrpc . "2.0")
                           (:id . ,(* trial 10))
                           (:method . "tools/call")
                           (:params . ((:name . "evaluate-lisp")
                                      (:arguments . ((:code . ,def-code)))))))))
               (is (result-response-p r1)))

             ;; Capture initial state
             (let ((initial-fn-check
                    (send-request server
                      `((:jsonrpc . "2.0")
                        (:id . ,(1+ (* trial 10)))
                        (:method . "tools/call")
                        (:params . ((:name . "evaluate-lisp")
                                   (:arguments . ((:code . ,(format nil "(list (fboundp '~A) (boundp '~A))"
                                                                   fn-name var-name))))))))))
               (is (result-response-p initial-fn-check))
               (is (search "T" (extract-result-text initial-fn-check)))

               ;; Use various introspection tools
               (dolist (tool *introspection-tools*)
                 (let ((args (case (intern (string-upcase tool) :keyword)
                              (:describe-symbol `((:name . ,fn-name)))
                              (:apropos-search `((:pattern . ,(subseq fn-name 0 5))))
                              (:who-calls `((:name . ,fn-name)))
                              (:who-references `((:name . ,var-name)))
                              (:macroexpand-form `((:form . ,(format nil "(~A)" fn-name))))
                              (:validate-syntax `((:code . ,(format nil "(~A)" fn-name)))))))
                   (send-request server
                     `((:jsonrpc . "2.0")
                       (:id . ,(+ 2 (* trial 10)))
                       (:method . "tools/call")
                       (:params . ((:name . ,tool)
                                  (:arguments . ,args)))))))

               ;; Verify state unchanged
               (let ((final-fn-check
                      (send-request server
                        `((:jsonrpc . "2.0")
                          (:id . ,(+ 3 (* trial 10)))
                          (:method . "tools/call")
                          (:params . ((:name . "evaluate-lisp")
                                     (:arguments . ((:code . ,(format nil "(list (fboundp '~A) (boundp '~A))"
                                                                     fn-name var-name))))))))))
                 (is (result-response-p final-fn-check))
                 (is (search "T" (extract-result-text final-fn-check))
                     "Definitions should still exist after introspection"))

               ;; Verify values unchanged
               (let ((value-check
                      (send-request server
                        `((:jsonrpc . "2.0")
                          (:id . ,(+ 4 (* trial 10)))
                          (:method . "tools/call")
                          (:params . ((:name . "evaluate-lisp")
                                     (:arguments . ((:code . ,var-name)))))))))
                 (is (search (format nil "~A" trial) (extract-result-text value-check))
                     "Variable value should be unchanged")))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Introspection Doesn't Create Side Effects

```lisp
(test introspection-no-side-effects-property
  "Introspection tools produce no observable side effects"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 30)
           ;; Create tracking variable
           (let ((r1 (send-request server
                       '((:jsonrpc . "2.0")
                         (:id . 1)
                         (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp")
                                    (:arguments . ((:code . "(defvar *side-effect-counter* 0)")))))))))
             (is (result-response-p r1)))

           ;; Define function with side effect
           (let ((r2 (send-request server
                       '((:jsonrpc . "2.0")
                         (:id . 2)
                         (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp")
                                    (:arguments . ((:code . "(defun side-effect-fn () (incf *side-effect-counter*))")))))))))
             (is (result-response-p r2)))

           ;; Use introspection tools many times
           (dotimes (i 10)
             ;; describe-symbol
             (send-request server
               '((:jsonrpc . "2.0")
                 (:id . 3)
                 (:method . "tools/call")
                 (:params . ((:name . "describe-symbol")
                            (:arguments . ((:name . "side-effect-fn")))))))

             ;; apropos-search
             (send-request server
               '((:jsonrpc . "2.0")
                 (:id . 4)
                 (:method . "tools/call")
                 (:params . ((:name . "apropos-search")
                            (:arguments . ((:pattern . "side-effect")))))))

             ;; validate-syntax
             (send-request server
               '((:jsonrpc . "2.0")
                 (:id . 5)
                 (:method . "tools/call")
                 (:params . ((:name . "validate-syntax")
                            (:arguments . ((:code . "(side-effect-fn)"))))))))

           ;; Counter should still be 0 (no actual calls)
           (let ((counter-check (send-request server
                                  '((:jsonrpc . "2.0")
                                    (:id . 6)
                                    (:method . "tools/call")
                                    (:params . ((:name . "evaluate-lisp")
                                               (:arguments . ((:code . "*side-effect-counter*")))))))))
             (is (search "0" (extract-result-text counter-check))
                 "Introspection should not trigger function calls"))

           ;; Reset for next trial
           (send-request server
             '((:jsonrpc . "2.0")
               (:id . 7)
               (:method . "tools/call")
               (:params . ((:name . "reset-session")
                          (:arguments . ()))))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Repeated Introspection Stability

```lisp
(test repeated-introspection-stability-property
  "Repeated introspection calls should return consistent results"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (progn
           ;; Define test function
           (let ((r1 (send-request server
                       '((:jsonrpc . "2.0")
                         (:id . 1)
                         (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp")
                                    (:arguments . ((:code . "(defun stability-test (x) (* x 2))")))))))))
             (is (result-response-p r1)))

           ;; Call describe-symbol 50 times
           (let ((results nil))
             (dotimes (i 50)
               (let ((response (send-request server
                                 '((:jsonrpc . "2.0")
                                   (:id . 2)
                                   (:method . "tools/call")
                                   (:params . ((:name . "describe-symbol")
                                              (:arguments . ((:name . "stability-test")))))))))
                 (is (result-response-p response))
                 (push (extract-result-text response) results)))

             ;; All results should be identical
             (let ((first-result (first results)))
               (dolist (result (rest results))
                 (is (string= first-result result)
                     "Repeated introspection should return identical results")))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Introspection on Nonexistent Symbols

```lisp
(test introspection-nonexistent-symbols-property
  "Introspecting nonexistent symbols doesn't crash or modify state"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (progn
           ;; Define known good state
           (let ((r1 (send-request server
                       '((:jsonrpc . "2.0")
                         (:id . 1)
                         (:method . "tools/call")
                         (:params . ((:name . "evaluate-lisp")
                                    (:arguments . ((:code . "(defvar *known-state* :stable)")))))))))
             (is (result-response-p r1)))

           ;; Try introspecting many nonexistent symbols
           (dotimes (i 100)
             (let* ((fake-name (format nil "NONEXISTENT-~A" i))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . 2)
                                  (:method . "tools/call")
                                  (:params . ((:name . "describe-symbol")
                                             (:arguments . ((:name . ,fake-name)))))))))
               (is (result-response-p response)
                   "Should return valid response for nonexistent symbol")))

           ;; Verify known state unchanged
           (let ((state-check (send-request server
                                '((:jsonrpc . "2.0")
                                  (:id . 3)
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . "*known-state*")))))))))
             (is (search "STABLE" (extract-result-text state-check) :test #'char-equal)
                 "State should be unchanged after failed introspection")))
      (cl-mcp-server:stop-test-server server))))
```

## Configuration

- Trials: 50 for normal runs
- Trials: 200+ for thorough testing

## Assertions

For all introspection tools:
1. Session state must remain unchanged after introspection
2. No functions should be called as side effects
3. Variable values must not change
4. Repeated calls must return consistent results
5. Failed introspection (nonexistent symbols) must not corrupt state
6. All tools must be truly read-only

## Notes

- Critical safety property for introspection tools
- Ensures Claude can explore code without risk
- Verifies no accidental execution during introspection
- Tests both existing and nonexistent symbol cases
