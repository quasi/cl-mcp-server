---
type: property
name: structured-error-response
version: 1.0.0
feature: error-handling
covers:
  - contracts/condition-report
---

# Structured Error Response Property

## Statement

**For all** evaluation errors,
**the error response** MUST follow the structured format defined in the condition-report contract,
**including** condition type, message, and backtrace sections.

## Formal Expression

```
∀ error ∈ EvaluationErrors :
  let response = format_error(error) in
    response.isError = true ∧
    has_section(response, "ERROR") ∧
    has_section(response, "Backtrace") ∧
    section_order_valid(response) ∧
    condition_type_present(response) ∧
    message_present(response)

where section_order is:
  [stdout]? → [ERROR] TYPE → message → [Backtrace]
```

## Informal Explanation

Error responses must follow a consistent, parseable structure:

```
[stdout]                          ← Optional: if code produced output before error
{captured output}

[ERROR] {CONDITION-TYPE}          ← Required: condition class name
{human-readable message}          ← Required: condition description

[Backtrace]                       ← Required: stack trace
0: (FOO ...)
1: (BAR ...)
...
```

The structure must be:
1. **Consistent**: Same format for all error types
2. **Parseable**: Section markers allow Claude to extract components
3. **Complete**: All required information present
4. **Ordered**: Sections appear in predictable sequence

This differs from warning responses, which do not set `isError: true` and use `[warnings]` section instead.

## Rationale

Structured error responses enable Claude to:
- **Parse components**: Extract type, message, and backtrace separately
- **Present errors clearly**: Show errors with appropriate formatting
- **Diagnose issues**: Use type and backtrace for accurate suggestions
- **Handle errors programmatically**: Apply different strategies based on error type

Ad-hoc formatting makes errors harder to parse and understand. Consistency across all errors reduces cognitive load for Claude and users.

## Counterexample Shape

If this property is violated, you might see:

**Missing Sections**:
```
[ERROR] TYPE-ERROR
The value "hello" is not of type NUMBER.
```
(Missing backtrace section)

**Wrong Order**:
```
[Backtrace]
0: (+ 1 "hello")

[ERROR] TYPE-ERROR
The value "hello" is not of type NUMBER.
```
(Backtrace before error marker)

**Missing Type**:
```
[ERROR]
The value "hello" is not of type NUMBER.

[Backtrace]
...
```
(Condition type omitted)

**Unparseable Format**:
```
ERROR: TYPE-ERROR - The value "hello" is not of type NUMBER.
Stack: (+ 1 "hello")
```
(Non-standard section markers)

**Unstructured Text**:
```
An error occurred: The value "hello" is not of type NUMBER.
```
(No structure, just prose)

## Verification Approach

**Template Matching**: Define expected structure and verify conformance

```lisp
(defun parse-error-response (text)
  "Parse error response into structured components. Returns NIL if malformed."
  (let ((sections (make-hash-table :test 'equal)))
    ;; Look for optional stdout section
    (when (search "[stdout]" text)
      (setf (gethash :stdout sections)
            (extract-section text "[stdout]" "[ERROR]")))

    ;; Look for required ERROR section
    (unless (search "[ERROR]" text)
      (return-from parse-error-response nil))

    (let* ((error-start (search "[ERROR]" text))
           (error-line-end (position #\Newline text :start error-start))
           (type-part (subseq text (+ error-start 8) error-line-end)))
      (when (zerop (length (string-trim " " type-part)))
        (return-from parse-error-response nil))
      (setf (gethash :type sections) (string-trim " " type-part)))

    ;; Extract message (between type and backtrace)
    (let* ((msg-start (position #\Newline text :start (search "[ERROR]" text)))
           (bt-start (search "[Backtrace]" text)))
      (unless bt-start
        (return-from parse-error-response nil))
      (setf (gethash :message sections)
            (string-trim '(#\Newline #\Space)
                        (subseq text msg-start bt-start))))

    ;; Extract backtrace
    (let ((bt-start (search "[Backtrace]" text)))
      (setf (gethash :backtrace sections)
            (subseq text (+ bt-start 11))))

    sections))

(defun verify-structured-error-response (error-code)
  (let ((response (evaluate-lisp error-code)))
    ;; Must be error response
    (assert (error-response-p response))

    (let ((text (response-text response))
          (parsed (parse-error-response (response-text response))))

      ;; Must be parseable
      (assert parsed
              nil
              "Error response not parseable: ~A" text)

      ;; Must have all required sections
      (assert (gethash :type parsed)
              nil
              "Missing condition type in: ~A" text)

      (assert (gethash :message parsed)
              nil
              "Missing message in: ~A" text)

      (assert (gethash :backtrace parsed)
              nil
              "Missing backtrace in: ~A" text)

      ;; Type must be non-empty
      (assert (plusp (length (gethash :type parsed)))
              nil
              "Empty condition type in: ~A" text)

      ;; Message must be non-empty
      (assert (plusp (length (gethash :message parsed)))
              nil
              "Empty message in: ~A" text)

      ;; Backtrace must contain frame markers
      (assert (search "0:" (gethash :backtrace parsed))
              nil
              "Backtrace malformed (no frame 0) in: ~A" text))))
```

**Property Test**:
1. Generate diverse error-producing code
2. For each error response:
   - Verify `isError: true`
   - Parse using `parse-error-response`
   - Verify parsing succeeds
   - Verify all required sections present
   - Verify section contents non-empty
   - Verify sections appear in correct order
3. Test 100+ different error types
4. Verify all follow same structure

**Format Invariants**:
```lisp
(defun verify-format-invariants (error-responses)
  (dolist (response error-responses)
    (let ((text (response-text response)))
      ;; [ERROR] appears before [Backtrace]
      (assert (< (search "[ERROR]" text)
                 (search "[Backtrace]" text)))

      ;; Condition type on same line as [ERROR]
      (let* ((error-pos (search "[ERROR]" text))
             (newline-pos (position #\Newline text :start error-pos)))
        (assert (> newline-pos (+ error-pos 8))))

      ;; Backtrace contains numbered frames
      (let ((bt-section (subseq text (search "[Backtrace]" text))))
        (assert (search "0:" bt-section))
        (assert (search "(" bt-section)))  ; at least one frame

      ;; If stdout present, it comes first
      (when (search "[stdout]" text)
        (assert (< (search "[stdout]" text)
                   (search "[ERROR]" text)))))))
```

**Edge Cases**:
- Errors with empty messages (use default text)
- Errors with no backtrace available (show placeholder)
- Errors with stdout output before error (test section ordering)
- Very long error messages (test truncation)
- Error messages containing section markers (test escaping)
- Errors in non-ASCII encodings (test unicode handling)

**Regression Test**:
Create snapshot tests for known error formats:
```lisp
(define-error-format-test undefined-function
  :code "(nonexistent-fn 1 2)"
  :expected-pattern "\\[ERROR\\] UNDEFINED-FUNCTION.*\\[Backtrace\\]")

(define-error-format-test type-error
  :code "(+ 1 \"hello\")"
  :expected-pattern "\\[ERROR\\] TYPE-ERROR.*is not of type.*\\[Backtrace\\]")
```

**Shrinking**: Find minimal error that produces malformed response structure
