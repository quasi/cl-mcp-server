---
type: verification
name: error-capture-completeness-property-test
source: properties/error-capture-completeness.md
level: property
tags:
  - property-based
  - error-handling
  - completeness
---

# Property Test: Error Capture Completeness

## Purpose

Verify that all errors produce complete diagnostic information including type, message, backtrace, and restarts.

## Prerequisites

- Initialized MCP server
- Error-intelligence tools available

## Implementation

### Generator: Error-Producing Code

```lisp
(defun generate-error-code ()
  "Generate code that produces various error types"
  (random-choice
   '(;; Arithmetic errors
     "(/ 1 0)"
     "(mod 10 0)"
     "(sqrt -1)"

     ;; Type errors
     "(car 'not-a-list)"
     "(+ 1 \"string\")"
     "(elt '(1 2 3) \"not-integer\")"

     ;; Undefined function
     "(nonexistent-function 42)"
     "(funcall 'undefined-func)"

     ;; Unbound variable
     "undefined-variable"
     "*nonexistent-special*"

     ;; User errors
     "(error \"test error\")"
     "(error 'simple-error :format-control \"test\")"

     ;; Index errors
     "(elt '(1 2 3) 999)"
     "(aref #(1 2 3) 10)")))
```

### Property: All Errors Captured

```lisp
(deftest error-capture-all-errors ()
  "Every error produces error information"
  (dotimes (i 100)
    (let* ((code (generate-error-code))
           ;; Trigger error
           (eval-response (call-tool *test-server* "evaluate-lisp"
                                    `(("code" . ,code))))
           ;; Get error info
           (error-response (call-tool *test-server* "describe-last-error" '())))

      ;; Evaluation should error
      (is (or (search "error" (result-content eval-response) :test #'char-equal)
              (search "Error" (result-content eval-response)))
          "Code didn't error: ~A" code)

      ;; Error info available
      (is (result-response-p error-response)
          "describe-last-error failed for code: ~A" code)

      ;; Has error type
      (let ((content (result-content error-response)))
        (is (some (lambda (type)
                    (search type content :test #'char-equal))
                  '("ERROR" "DIVISION-BY-ZERO" "TYPE-ERROR"
                    "UNDEFINED-FUNCTION" "UNBOUND-VARIABLE"))
            "No error type in response for code: ~A" code)))))
```

### Property: Error Type Specific

```lisp
(deftest error-capture-specific-types ()
  "Error types are specific, not generic"
  (let ((specific-errors
         '(("(/ 1 0)" "DIVISION-BY-ZERO")
           ("(car 'not-a-list)" "TYPE-ERROR")
           ("(nonexistent-func)" "UNDEFINED-FUNCTION")
           ("undefined-var" "UNBOUND-VARIABLE"))))

    (dolist (case specific-errors)
      (destructuring-bind (code expected-type) case
        (call-tool *test-server* "evaluate-lisp" `(("code" . ,code)))
        (let* ((response (call-tool *test-server* "describe-last-error" '()))
               (content (result-content response)))

          (is (search expected-type content :test #'char-equal)
              "Expected ~A for code ~A" expected-type code))))))
```

### Property: Backtrace Present

```lisp
(deftest error-capture-backtrace-present ()
  "All errors have backtrace information"
  (dotimes (i 50)
    (let* ((code (generate-error-code)))
      ;; Trigger error
      (call-tool *test-server* "evaluate-lisp" `(("code" . ,code)))

      ;; Check backtrace
      (let* ((response (call-tool *test-server* "describe-last-error" '()))
             (content (result-content response)))

        (is (search "Backtrace" content :test #'char-equal)
            "No backtrace for code: ~A" code)

        ;; Should have frame information
        (is (or (search "frame" content :test #'char-equal)
                (search "0:" content)
                (search "1:" content))
            "No frame info in backtrace for code: ~A" code)))))
```

### Property: Restarts Available

```lisp
(deftest error-capture-restarts ()
  "Error info includes available restarts"
  (dotimes (i 50)
    (let* ((code (generate-error-code)))
      ;; Trigger error
      (call-tool *test-server* "evaluate-lisp" `(("code" . ,code)))

      ;; Check restarts
      (let* ((response (call-tool *test-server* "describe-last-error" '()))
             (content (result-content response)))

        (is (search "Restart" content :test #'char-equal)
            "No restart section for code: ~A" code)

        ;; At least ABORT should be available
        (is (search "ABORT" content :test #'char-equal)
            "No ABORT restart for code: ~A" code)))))
```

### Property: Nested Call Frames

```lisp
(deftest error-capture-nested-frames ()
  "Nested calls show complete call chain"
  (let ((nested-code
         "(defun a () (/ 1 0))
          (defun b () (a))
          (defun c () (b))
          (c)"))

    (call-tool *test-server* "evaluate-lisp" `(("code" . ,nested-code)))

    (let* ((response (call-tool *test-server* "describe-last-error" '()))
           (content (result-content response)))

      ;; Should show multiple frames
      (is (search "Backtrace" content :test #'char-equal))

      ;; Should mention the functions
      (is (or (search "A" content)
              (search "B" content)
              (search "C" content))
          "Nested functions not in backtrace"))))
```

### Property: Error Message Present

```lisp
(deftest error-capture-message ()
  "All errors have human-readable message"
  (dotimes (i 50)
    (let* ((code (generate-error-code)))
      (call-tool *test-server* "evaluate-lisp" `(("code" . ,code)))

      (let* ((response (call-tool *test-server* "describe-last-error" '()))
             (content (result-content response)))

        ;; Should have error message text
        (is (> (length content) 50)
            "Error message too short for code: ~A" code)

        ;; Should not be empty
        (is (not (search "No error" content :test #'char-equal))
            "No error captured for code: ~A" code)))))
```

### Property: Custom Error Messages

```lisp
(deftest error-capture-custom-messages ()
  "User error messages captured correctly"
  (dotimes (i 20)
    (let* ((message (format nil "Test error ~A" i))
           (code (format nil "(error \"~A\")" message)))

      (call-tool *test-server* "evaluate-lisp" `(("code" . ,code)))

      (let* ((response (call-tool *test-server* "describe-last-error" '()))
             (content (result-content response)))

        ;; Custom message should appear
        (is (search message content :test #'string=)
            "Custom message not found: ~A" message)))))
```

## Configuration

- Examples: 50-100 per test
- Cover all common error types
- Test both system and user errors

## Helper Functions

```lisp
(defun random-choice (list)
  "Pick random element from list"
  (nth (random (length list)) list))
```

## Notes

- Every error must produce diagnostic information
- Error types must be specific (not just "ERROR")
- Backtrace and restarts always required
- Custom error messages must be preserved
