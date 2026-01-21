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

;;; with-error-capture tests

(test with-error-capture-success
  "with-error-capture returns values on success"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (+ 1 2))
    (is (equal '(3) result))
    (is (null error-string))
    (is (null warnings))))

(test with-error-capture-multiple-values
  "with-error-capture preserves multiple values"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (values 1 2 3))
    (is (equal '(1 2 3) result))
    (is (null error-string))
    (is (null warnings))))

(test with-error-capture-handles-error
  "with-error-capture captures errors"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (error "Test error"))
    (is (null result))
    (is (stringp error-string))
    (is (search "Test error" error-string))
    (is (null warnings))))

(test with-error-capture-collects-warnings
  "with-error-capture collects warnings"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (warn "Warning 1")
        (warn "Warning 2")
        42)
    (is (equal '(42) result))
    (is (null error-string))
    (is (= 2 (length warnings)))
    (is (every #'stringp warnings))))

(test with-error-capture-error-and-warnings
  "with-error-capture captures both error and warnings"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (warn "Warning before error")
        (error "Boom"))
    (is (null result))
    (is (stringp error-string))
    (is (search "Boom" error-string))
    ;; Warnings collected before error should still be captured
    (is (= 1 (length warnings)))))

;;; Specific condition type tests

(test condition-unbound-variable
  "Handles unbound variable condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (symbol-value 'nonexistent-variable-12345))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    (is (search "UNBOUND-VARIABLE" error-string))))

(test condition-division-by-zero
  "Handles division by zero condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (/ 1 0))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    (is (search "DIVISION-BY-ZERO" error-string))))

(test condition-undefined-function
  "Handles undefined function condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (funcall 'nonexistent-function-12345))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    (is (search "UNDEFINED-FUNCTION" error-string))))

(test condition-type-error
  "Handles type error condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (+ "not a number" 1))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    (is (search "TYPE-ERROR" error-string))))

(test condition-package-error
  "Handles package error condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (find-package 'nonexistent-package-12345)
        ;; find-package returns nil for nonexistent, use intern instead
        (intern "FOO" 'nonexistent-package-12345))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    ;; Package errors can be various types
    (is (or (search "PACKAGE-ERROR" error-string)
            (search "PACKAGE" error-string)))))

(test condition-file-error
  "Handles file error condition"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (open "/nonexistent/path/file.txt"))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    ;; File errors show up differently on different systems
    (is (or (search "FILE-ERROR" error-string)
            (search "file" error-string)
            (search "No such file" error-string)))))

(test condition-mcp-error
  "Handles custom MCP error conditions"
  (multiple-value-bind (result error-string warnings)
      (cl-mcp-server.error-format:with-error-capture
        (error 'cl-mcp-server.conditions:invalid-params
               :message "Bad parameter"))
    (declare (ignore warnings))
    (is (null result))
    (is (stringp error-string))
    (is (search "INVALID-PARAMS" error-string))
    (is (search "Bad parameter" error-string))))
