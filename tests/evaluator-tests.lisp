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
