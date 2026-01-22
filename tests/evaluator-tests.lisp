;;; tests/evaluator-tests.lisp
;;; ABOUTME: Tests for code evaluator

(in-package #:cl-mcp-server-tests)

(def-suite evaluator-tests
  :description "Code evaluator tests"
  :in cl-mcp-server-tests)

(in-suite evaluator-tests)

;;; ==========================================================================
;;; Task 1: Evaluation Result Structure Tests
;;; ==========================================================================

(def-suite evaluation-result-tests
  :description "Tests for evaluation-result structure"
  :in evaluator-tests)

(in-suite evaluation-result-tests)

(test make-evaluation-result-success
  "Test creating a successful evaluation result"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                 :success-p t
                 :values '("42"))))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("42") (cl-mcp-server.evaluator:result-values result)))
    (is (equal "" (cl-mcp-server.evaluator:result-stdout result)))
    (is (equal "" (cl-mcp-server.evaluator:result-stderr result)))
    (is (null (cl-mcp-server.evaluator:result-warnings result)))
    (is (null (cl-mcp-server.evaluator:result-error result)))))

(test make-evaluation-result-error
  "Test creating an error evaluation result"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                 :success-p nil
                 :error-info "Division by zero")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (null (cl-mcp-server.evaluator:result-values result)))
    (is (equal "Division by zero" (cl-mcp-server.evaluator:result-error result)))))

(test make-evaluation-result-with-output
  "Test creating result with stdout and stderr"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                 :success-p t
                 :values '("NIL")
                 :stdout "Hello, World!"
                 :stderr "Warning: something")))
    (is (equal "Hello, World!" (cl-mcp-server.evaluator:result-stdout result)))
    (is (equal "Warning: something" (cl-mcp-server.evaluator:result-stderr result)))))

(test make-evaluation-result-with-warnings
  "Test creating result with warnings"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                 :success-p t
                 :values '("done")
                 :warnings '("Undefined variable X"))))
    (is (equal '("Undefined variable X")
               (cl-mcp-server.evaluator:result-warnings result)))))

(test evaluation-result-defaults
  "Test that defaults are applied correctly"
  (let ((result (cl-mcp-server.evaluator:make-evaluation-result
                 :success-p t)))
    (is (equal "" (cl-mcp-server.evaluator:result-stdout result)))
    (is (equal "" (cl-mcp-server.evaluator:result-stderr result)))
    (is (null (cl-mcp-server.evaluator:result-values result)))
    (is (null (cl-mcp-server.evaluator:result-warnings result)))
    (is (null (cl-mcp-server.evaluator:result-error result)))))

;;; ==========================================================================
;;; Task 2: Basic Code Evaluation Tests
;;; ==========================================================================

(def-suite basic-evaluation-tests
  :description "Tests for basic code evaluation"
  :in evaluator-tests)

(in-suite basic-evaluation-tests)

(test evaluate-simple-expression
  "Test evaluating a simple arithmetic expression"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2 3)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("6") (cl-mcp-server.evaluator:result-values result)))
    (is (null (cl-mcp-server.evaluator:result-error result)))))

(test evaluate-multiple-values
  "Test evaluating code that returns multiple values"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(values 1 2 3)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("1" "2" "3") (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-nil-result
  "Test evaluating code that returns NIL"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "nil")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("NIL") (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-multiple-forms
  "Test evaluating multiple forms (returns last value)"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(setq x 10) (* x 2)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("20") (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-string-result
  "Test evaluating code that returns a string"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "\"hello, world\"")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("\"hello, world\"")
               (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-list-result
  "Test evaluating code that returns a list"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "'(a b c)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("(A B C)") (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-symbol-result
  "Test evaluating code that returns a symbol"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "'foo")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("FOO") (cl-mcp-server.evaluator:result-values result)))))

(test evaluate-no-values
  "Test evaluating code that returns no values"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(values)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (null (cl-mcp-server.evaluator:result-values result)))))
