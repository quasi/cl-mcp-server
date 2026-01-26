---
type: verification
name: session-lifecycle-property-test
source: properties/session-lifecycle.md
level: property
tags:
  - property-based
  - session
  - lifecycle
---

# Property Test: Session Lifecycle

## Purpose

Verify session lifecycle properties: initialization, accumulation, reset, and reuse.

## Prerequisites

- Initialized server
- Property-based testing framework

## Implementation

### Property: Reset Clears All State

```lisp
(test reset-clears-all-state-property
  "After reset, no user-defined symbols should be accessible"
  (let ((server (make-initialized-test-server))
        (defined-symbols nil))
    (unwind-protect
         (progn
           ;; Define random set of symbols
           (dotimes (i (+ 10 (random 20)))
             (let* ((name (format nil "RESET-TEST-~A" i))
                    (def-type (random-choice '(defun defvar defparameter)))
                    (code (case def-type
                            (defun (format nil "(defun ~A (x) x)" name))
                            (defvar (format nil "(defvar *~A* ~A)" name i))
                            (defparameter (format nil "(defparameter *~A* ~A)" name i)))))
               (let ((response (send-request server
                                 `((:jsonrpc . "2.0")
                                   (:id . ,i)
                                   (:method . "tools/call")
                                   (:params . ((:name . "evaluate-lisp")
                                              (:arguments . ((:code . ,code)))))))))
                 (when (result-response-p response)
                   (push (list (intern name) def-type) defined-symbols)))))

           ;; Reset session
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 1000)
                               (:method . "tools/call")
                               (:params . ((:name . "reset-session")
                                          (:arguments . ())))))))
             (is (result-response-p response)))

           ;; Verify all symbols are cleared
           (dolist (sym-info defined-symbols)
             (let* ((sym (first sym-info))
                    (def-type (second sym-info))
                    (check-code (case def-type
                                  (defun (format nil "(fboundp '~A)" sym))
                                  ((defvar defparameter) (format nil "(boundp '~A)" sym))))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . 2000)
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,check-code)))))))))
               (is (result-response-p response))
               (is (or (search "NIL" (extract-result-text response))
                       (search "error" (extract-result-text response) :test #'char-equal))
                   "Symbol ~A should not be accessible after reset" sym))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Post-Reset Functionality

```lisp
(test post-reset-functionality-property
  "After reset, session should be fully functional"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (cycle 10)
           ;; Define some state
           (let* ((name (format nil "CYCLE-~A-FN" cycle))
                  (code (format nil "(defun ~A () ~A)" name cycle)))
             (let ((response (send-request server
                               `((:jsonrpc . "2.0")
                                 (:id . ,(* cycle 3))
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . ,code)))))))))
               (is (result-response-p response))))

           ;; Reset
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 1)
                               (:method . "tools/call")
                               (:params . ((:name . "reset-session")
                                          (:arguments . ())))))))
             (is (result-response-p response)))

           ;; Define new state (should work)
           (let* ((new-name (format nil "POST-RESET-~A" cycle))
                  (new-code (format nil "(defun ~A (x) (* x ~A))" new-name (1+ cycle))))
             (let ((response (send-request server
                               `((:jsonrpc . "2.0")
                                 (:id . ,(1+ (* cycle 3)))
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . ,new-code)))))))))
               (is (result-response-p response)))

             ;; Use new definition
             (let* ((use-code (format nil "(~A 10)" new-name))
                    (expected-result (* 10 (1+ cycle)))
                    (response (send-request server
                                `((:jsonrpc . "2.0")
                                  (:id . ,(+ 2 (* cycle 3)))
                                  (:method . "tools/call")
                                  (:params . ((:name . "evaluate-lisp")
                                             (:arguments . ((:code . ,use-code)))))))))
               (is (result-response-p response))
               (is (search (format nil "~A" expected-result) (extract-result-text response))
                   "Post-reset function should work correctly"))))
      (cl-mcp-server:stop-test-server server))))
```

### Property: Package Context Restoration

```lisp
(test package-context-restoration-property
  "After reset, package should be CL-USER"
  (let ((server (make-initialized-test-server)))
    (unwind-protect
         (dotimes (trial 20)
           ;; Create custom package
           (let* ((pkg-name (format nil "TEST-PKG-~A" trial))
                  (code (format nil "(defpackage :~A (:use :cl))" pkg-name)))
             (let ((response (send-request server
                               `((:jsonrpc . "2.0")
                                 (:id . ,(* trial 3))
                                 (:method . "tools/call")
                                 (:params . ((:name . "evaluate-lisp")
                                            (:arguments . ((:code . ,code)))))))))
               (is (result-response-p response))))

           ;; Switch to it
           (let* ((switch-code (format nil "(in-package :TEST-PKG-~A)" trial))
                  (response (send-request server
                              `((:jsonrpc . "2.0")
                                (:id . ,(1+ (* trial 3)))
                                (:method . "tools/call")
                                (:params . ((:name . "evaluate-lisp")
                                           (:arguments . ((:code . ,switch-code)))))))))
             (is (result-response-p response)))

           ;; Reset
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 1000)
                               (:method . "tools/call")
                               (:params . ((:name . "reset-session")
                                          (:arguments . ())))))))
             (is (result-response-p response)))

           ;; Verify back in CL-USER
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 2000)
                               (:method . "tools/call")
                               (:params . ((:name . "evaluate-lisp")
                                          (:arguments . ((:code . "(package-name *package*)")))))))))
             (is (result-response-p response))
             (is (search "CL-USER" (extract-result-text response))
                 "Package should be CL-USER after reset")))
      (cl-mcp-server:stop-test-server server))))
```

### Property: list-definitions Accuracy

```lisp
(test list-definitions-accuracy-property
  "list-definitions must accurately reflect current session state"
  (let ((server (make-initialized-test-server))
        (expected-symbols nil))
    (unwind-protect
         (progn
           ;; Define random symbols
           (dotimes (i (+ 5 (random 10)))
             (let* ((name (format nil "LIST-TEST-~A" i))
                    (def-type (random-choice '(defun defvar)))
                    (code (case def-type
                            (defun (format nil "(defun ~A () ~A)" name i))
                            (defvar (format nil "(defvar *~A* ~A)" name i)))))
               (let ((response (send-request server
                                 `((:jsonrpc . "2.0")
                                   (:id . ,i)
                                   (:method . "tools/call")
                                   (:params . ((:name . "evaluate-lisp")
                                              (:arguments . ((:code . ,code)))))))))
                 (when (result-response-p response)
                   (push name expected-symbols)))))

           ;; Get list-definitions
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 1000)
                               (:method . "tools/call")
                               (:params . ((:name . "list-definitions")
                                          (:arguments . ())))))))
             (is (result-response-p response))
             (let ((text (extract-result-text response)))
               ;; All defined symbols should appear
               (dolist (name expected-symbols)
                 (is (search name text :test #'char-equal)
                     "Symbol ~A should appear in list-definitions" name))))

           ;; Reset and verify list is empty
           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 2000)
                               (:method . "tools/call")
                               (:params . ((:name . "reset-session")
                                          (:arguments . ())))))))
             (is (result-response-p response)))

           (let ((response (send-request server
                             '((:jsonrpc . "2.0")
                               (:id . 3000)
                               (:method . "tools/call")
                               (:params . ((:name . "list-definitions")
                                          (:arguments . ())))))))
             (is (result-response-p response))
             (let ((text (extract-result-text response)))
               ;; Should indicate no definitions
               (is (or (search "no" text :test #'char-equal)
                       (< (length text) 100))
                   "list-definitions should show empty state after reset"))))
      (cl-mcp-server:stop-test-server server))))
```

## Configuration

- Trials: 20 for normal runs
- Trials: 100 for thorough testing

## Assertions

1. After reset, all user-defined symbols must be inaccessible
2. After reset, new definitions must work normally
3. After reset, package must be CL-USER
4. list-definitions must accurately reflect current state
5. Multiple reset cycles must not corrupt the session

## Notes

- Tests the complete lifecycle: define → use → reset → redefine
- Verifies reset is complete and doesn't leave residual state
- Ensures session remains functional after reset
- Tests list-definitions as a state query tool
