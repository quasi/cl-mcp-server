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

;;; ==========================================================================
;;; Task 3: Output Capture Tests
;;; ==========================================================================

(def-suite output-capture-tests
  :description "Tests for output stream capture"
  :in evaluator-tests)

(in-suite output-capture-tests)

(test capture-stdout-print
  "Test capturing stdout from print"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(print \"hello\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (search "hello" (cl-mcp-server.evaluator:result-stdout result)))))

(test capture-stdout-princ
  "Test capturing stdout from princ"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(princ \"hello\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "hello" (cl-mcp-server.evaluator:result-stdout result)))))

(test capture-stdout-format
  "Test capturing stdout from format"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(format t \"Hello, ~a!\" \"world\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "Hello, world!" (cl-mcp-server.evaluator:result-stdout result)))))

(test capture-stdout-write-string
  "Test capturing stdout from write-string"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(write-string \"line1\") (write-string \"line2\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "line1line2" (cl-mcp-server.evaluator:result-stdout result)))))

(test capture-stderr
  "Test capturing stderr from *error-output*"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(format *error-output* \"error message\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "error message" (cl-mcp-server.evaluator:result-stderr result)))))

(test capture-both-streams
  "Test capturing both stdout and stderr"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(princ \"out\") (format *error-output* \"err\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "out" (cl-mcp-server.evaluator:result-stdout result)))
    (is (equal "err" (cl-mcp-server.evaluator:result-stderr result)))))

(test capture-no-output
  "Test that code without output has empty stdout/stderr"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "" (cl-mcp-server.evaluator:result-stdout result)))
    (is (equal "" (cl-mcp-server.evaluator:result-stderr result)))))

;;; ==========================================================================
;;; Task 4: Warning Capture Tests
;;; ==========================================================================

(def-suite warning-capture-tests
  :description "Tests for warning capture"
  :in evaluator-tests)

(in-suite warning-capture-tests)

(test capture-simple-warning
  "Test capturing a simple warning"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(warn \"Something might be wrong\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (= 1 (length (cl-mcp-server.evaluator:result-warnings result))))
    (is (search "Something might be wrong"
                (first (cl-mcp-server.evaluator:result-warnings result))))))

(test capture-multiple-warnings
  "Test capturing multiple warnings"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(warn \"Warning 1\") (warn \"Warning 2\") (warn \"Warning 3\")")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (= 3 (length (cl-mcp-server.evaluator:result-warnings result))))))

(test warnings-with-successful-result
  "Test that warnings don't prevent successful result"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(warn \"warning\") (+ 1 2)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("3") (cl-mcp-server.evaluator:result-values result)))
    (is (= 1 (length (cl-mcp-server.evaluator:result-warnings result))))))

(test no-warnings
  "Test that code without warnings has empty warnings list"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2)")))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (null (cl-mcp-server.evaluator:result-warnings result)))))

;;; ==========================================================================
;;; Task 5: Error Handling Tests
;;; ==========================================================================

(def-suite error-handling-tests
  :description "Tests for error handling during evaluation"
  :in evaluator-tests)

(in-suite error-handling-tests)

(test handle-undefined-function
  "Test handling undefined function errors"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(nonexistent-function 1 2 3)")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (null (cl-mcp-server.evaluator:result-values result)))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))
    (is (search "UNDEFINED-FUNCTION"
                (cl-mcp-server.evaluator:result-error result)))))

(test handle-unbound-variable
  "Test handling unbound variable errors"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "nonexistent-variable")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))
    (is (search "UNBOUND-VARIABLE"
                (cl-mcp-server.evaluator:result-error result)))))

(test handle-type-error
  "Test handling type errors"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(car 123)")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))
    (is (search "TYPE-ERROR"
                (cl-mcp-server.evaluator:result-error result)))))

(test handle-division-by-zero
  "Test handling division by zero"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(/ 10 0)")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))
    (is (search "DIVISION-BY-ZERO"
                (cl-mcp-server.evaluator:result-error result)))))

(test handle-reader-error
  "Test handling reader errors (malformed input)"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))))

(test handle-reader-error-extra-parens
  "Test handling extra closing parentheses"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2))")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))))

(test error-preserves-output
  "Test that output before error is still captured"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(princ \"before error\") (error \"intentional error\")")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "before error" (cl-mcp-server.evaluator:result-stdout result)))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))))

(test error-preserves-warnings
  "Test that warnings before error are still captured"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(warn \"warning before error\") (error \"intentional error\")")))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (= 1 (length (cl-mcp-server.evaluator:result-warnings result))))
    (is (stringp (cl-mcp-server.evaluator:result-error result)))))

;;; ==========================================================================
;;; Task 6: Result Formatting Tests
;;; ==========================================================================

(def-suite result-formatting-tests
  :description "Tests for formatting results for MCP"
  :in evaluator-tests)

(in-suite result-formatting-tests)

(test format-simple-result
  "Test formatting a simple successful result"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2)"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (stringp formatted))
    (is (search "=> 3" formatted))))

(test format-multiple-values-result
  "Test formatting a result with multiple values"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code "(values 1 2 3)"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (search "=> 1" formatted))
    (is (search "=> 2" formatted))
    (is (search "=> 3" formatted))))

(test format-result-with-output
  "Test formatting a result with stdout output"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code
                  "(princ \"hello\") 42"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (search "hello" formatted))
    (is (search "=> 42" formatted))))

(test format-result-with-warnings
  "Test formatting a result with warnings"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code
                  "(warn \"be careful\") :ok"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (search "Warning" formatted))
    (is (search "be careful" formatted))
    (is (search "=> :OK" formatted))))

(test format-error-result
  "Test formatting an error result"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code "(error \"oops\")"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (search "[ERROR]" formatted))
    (is (search "oops" formatted))))

(test format-no-values-result
  "Test formatting a result with no values"
  (let* ((result (cl-mcp-server.evaluator:evaluate-code "(values)"))
         (formatted (cl-mcp-server.evaluator:format-result result)))
    (is (search "; No values" formatted))))

;;; ==========================================================================
;;; Task 7: Timeout Tests
;;; ==========================================================================

(def-suite timeout-tests
  :description "Tests for evaluation timeout protection"
  :in evaluator-tests)

(in-suite timeout-tests)

(test default-timeout-value
  "Test that default timeout is 30 seconds"
  (is (= 30 cl-mcp-server.evaluator:*evaluation-timeout*)))

(test default-max-output-value
  "Test that default max output is 100000 characters"
  (is (= 100000 cl-mcp-server.evaluator:*max-output-chars*)))

(test fast-code-succeeds-with-timeout
  "Test that fast code completes within timeout"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(+ 1 2)" :timeout 1)))
    (is-true (cl-mcp-server.evaluator:result-success-p result))
    (is (equal '("3") (cl-mcp-server.evaluator:result-values result)))))

(test slow-code-times-out
  "Test that slow code triggers timeout"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(sleep 3)" :timeout 1)))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (search "TIMEOUT" (cl-mcp-server.evaluator:result-error result)))))

(test timeout-error-contains-hint
  "Test that timeout error includes helpful hint"
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(sleep 2)" :timeout 1)))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (search "configure-limits" (cl-mcp-server.evaluator:result-error result)))))

(test nil-timeout-disables-limit
  "Test that NIL timeout disables the limit"
  ;; Short sleep should complete fine with no timeout
  (let ((result (cl-mcp-server.evaluator:evaluate-code "(sleep 0.1)" :timeout nil)))
    (is-true (cl-mcp-server.evaluator:result-success-p result))))

(test timeout-preserves-output
  "Test that output before timeout is captured"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(princ \"before sleep\") (sleep 2)" :timeout 1)))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (equal "before sleep" (cl-mcp-server.evaluator:result-stdout result)))))

(test timeout-preserves-warnings
  "Test that warnings before timeout are captured"
  (let ((result (cl-mcp-server.evaluator:evaluate-code
                 "(warn \"before timeout\") (sleep 2)" :timeout 1)))
    (is-false (cl-mcp-server.evaluator:result-success-p result))
    (is (= 1 (length (cl-mcp-server.evaluator:result-warnings result))))))
