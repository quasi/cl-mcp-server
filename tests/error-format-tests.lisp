;;; tests/error-format-tests.lisp
;;; ABOUTME: Tests for error formatting module

(in-package #:cl-mcp-server-tests)

(def-suite error-format-tests
  :description "Error formatting tests"
  :in cl-mcp-server-tests)

(in-suite error-format-tests)

;;; Configuration tests

(test default-backtrace-depth
  "Default backtrace depth is reasonable"
  (is (= 20 cl-mcp-server.error-format:*max-backtrace-depth*)))

(test default-print-backtrace
  "Backtraces enabled by default"
  (is (eq t cl-mcp-server.error-format:*print-backtrace-p*)))

;;; Condition type extraction tests

(test format-error-includes-type
  "format-error includes condition type"
  (let ((err (make-condition 'division-by-zero)))
    (is (search "DIVISION-BY-ZERO"
                (cl-mcp-server.error-format:format-error err)))))

(test format-error-includes-message
  "format-error includes condition message"
  (let ((err (make-condition 'simple-error
                             :format-control "Test message ~a"
                             :format-arguments '(42))))
    (is (search "Test message 42"
                (cl-mcp-server.error-format:format-error err)))))

(test format-error-includes-backtrace
  "format-error includes backtrace when enabled"
  (let ((cl-mcp-server.error-format:*print-backtrace-p* t))
    (let ((err (make-condition 'simple-error
                               :format-control "Test")))
      (is (search "[Backtrace]"
                  (cl-mcp-server.error-format:format-error err))))))

(test format-error-no-backtrace-when-disabled
  "format-error omits backtrace when disabled"
  (let ((cl-mcp-server.error-format:*print-backtrace-p* nil))
    (let ((err (make-condition 'simple-error
                               :format-control "Test")))
      (is (null (search "[Backtrace]"
                        (cl-mcp-server.error-format:format-error err)))))))

;;; Warning formatting tests

(test format-warning-basic
  "format-warning formats warnings"
  (let ((warn (make-condition 'simple-warning
                              :format-control "Test warning")))
    (let ((result (cl-mcp-server.error-format:format-warning warn)))
      (is (search "SIMPLE-WARNING" result))
      (is (search "Test warning" result)))))

;;; Generic format-condition tests

(test format-condition-dispatches-error
  "format-condition uses format-error for errors"
  (let ((err (make-condition 'simple-error
                             :format-control "Test")))
    (is (search "[ERROR]"
                (cl-mcp-server.error-format:format-condition err)))))

(test format-condition-dispatches-warning
  "format-condition uses format-warning for warnings"
  (let ((warn (make-condition 'simple-warning
                              :format-control "Test")))
    (let ((result (cl-mcp-server.error-format:format-condition warn)))
      ;; Warnings don't include [ERROR] prefix
      (is (null (search "[ERROR]" result)))
      (is (search "SIMPLE-WARNING" result)))))

;;; Backtrace truncation tests

(test backtrace-truncation
  "Backtrace is truncated to max depth"
  (let ((cl-mcp-server.error-format:*max-backtrace-depth* 5))
    ;; Create a deep call stack
    (labels ((deep-call (n)
               (if (zerop n)
                   (cl-mcp-server.error-format:format-backtrace)
                   (deep-call (1- n)))))
      (let* ((backtrace (deep-call 50))
             (truncated (cl-mcp-server.error-format::truncate-backtrace backtrace))
             (lines (count #\Newline truncated)))
        ;; Should have at most max-depth + 1 lines (including "...")
        (is (<= lines 6))))))
