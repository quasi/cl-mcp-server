---
type: property
name: error-state-preservation
version: 0.1.0
feature: error-intelligence
covers:
  - contracts/describe-last-error-tool
relates_to:
  - error-capture-completeness
  - backtrace-accuracy
---

# Error State Preservation Property

## Statement

**For all** errors that occur during evaluation,
**the error context** (type, message, backtrace, restarts) MUST be preserved and available via error intelligence tools until the next error occurs or the session is reset.

## Formal Expression

```
∀ code₁, code₂ ∈ Code :
  let t₀ = current_time()
  let error₁ = evaluate-lisp(code₁)  ; produces error at t₁
  let error₂ = evaluate-lisp(code₂)  ; may succeed or error at t₂

  when error₁.isError = true
  then:
    1. ∀ t ∈ [t₁, t₂) :
       describe-last-error(t) = error₁.context
       get-backtrace(t) = error₁.backtrace

    2. if error₂.isError = true
       then ∀ t ∈ [t₂, ∞) :
         describe-last-error(t) = error₂.context
         get-backtrace(t) = error₂.backtrace

    3. if error₂.isError = false
       then ∀ t ∈ [t₂, ∞) :
         describe-last-error(t) = NoError
         get-backtrace(t) = NoError

where:
  error.context = {
    type: String,
    message: String,
    restarts: [Restart],
    backtrace_preview: [Frame]
  }

  NoError indicates no error available
```

## Informal Explanation

Once an error occurs, all its diagnostic information must remain available for inspection until either:

1. **Another error occurs**: New error replaces the old one
2. **Successful evaluation**: Success clears the error state
3. **Session reset**: Explicit reset clears all state

Between error capture and state change, the error information must:
- **Persist**: Remain available across multiple tool calls
- **Remain unchanged**: Same information returned each time
- **Be complete**: No degradation or loss of detail

This allows Claude to:
- Examine error details multiple times
- Correlate information from different tools
- Build understanding incrementally
- Reference error context in follow-up questions

## Rationale

Error state preservation enables effective debugging workflows:

**Multi-step Diagnosis**:
- Get error overview with `describe-last-error`
- Examine full backtrace with `get-backtrace`
- Check related code based on stack frames
- Return to error details for confirmation

**Reasoning About Errors**:
- Compare error message with code expectations
- Trace call chain through source
- Evaluate different hypotheses
- All require stable error state

**User Interaction**:
- User asks "what went wrong?"
- User asks follow-up questions
- User wants to see specific stack frames
- All require persistent error context

Without preservation:
- Error details lost after first inspection
- Can't correlate information between tools
- Must reproduce error for each query
- Debugging becomes impractical

## Counterexample Shape

If this property is violated:

**Error Lost After First Query**:
```lisp
(evaluate-lisp "(/ 1 0)")
;; First call succeeds
(describe-last-error)
→ Returns error details

;; Second call fails
(describe-last-error)
→ "No error available"  ; VIOLATION! Error disappeared
```

**Error Details Change**:
```lisp
(evaluate-lisp "(/ 1 0)")

(let ((first-call (describe-last-error))
      (second-call (describe-last-error)))
  ;; VIOLATION! Details differ between calls
  (not (equal first-call second-call)))
```

**Backtrace Degrades**:
```lisp
(evaluate-lisp "(defun a () (/ 1 0)) (a)")

(get-backtrace :max-frames 10)
→ Shows 5 frames

;; Later...
(get-backtrace :max-frames 10)
→ Shows 3 frames  ; VIOLATION! Lost frames
```

**Success Doesn't Clear Error**:
```lisp
(evaluate-lisp "(/ 1 0)")
→ Error occurs

(evaluate-lisp "(+ 1 2)")
→ Success, returns 3

(describe-last-error)
→ Still shows division error  ; VIOLATION! Should clear
```

**Restarts Lost**:
```lisp
(evaluate-lisp "(error \"test\")")

;; First query
(describe-last-error)
→ Shows: restarts: [ABORT, RETRY]

;; Second query
(describe-last-error)
→ Shows: restarts: []  ; VIOLATION! Restarts disappeared
```

**Cross-Tool Inconsistency**:
```lisp
(evaluate-lisp "(defun foo () (error \"x\")) (foo)")

(describe-last-error)
→ Shows error type: SIMPLE-ERROR

(get-backtrace)
→ "No error available"  ; VIOLATION! State inconsistent
```

## Verification Approach

**Generator**: Generate sequences of operations (errors and successes)

**Assertion**:
```lisp
(defun verify-error-preservation (error-code)
  ;; Produce an error
  (let ((result (evaluate-lisp error-code)))
    (assert (result-is-error result))

    ;; Capture initial error state
    (let ((initial-error (describe-last-error))
          (initial-backtrace (get-backtrace)))

      (and
        ;; Error information available
        (error-available-p initial-error)
        (> (length initial-backtrace) 0)

        ;; Query multiple times - should be stable
        (loop repeat 10
              always (equal (describe-last-error) initial-error))

        (loop repeat 10
              always (equal (get-backtrace) initial-backtrace))

        ;; Different tools show consistent state
        (let ((error-type (extract-type initial-error))
              (bt-has-frames (> (length initial-backtrace) 0)))
          (and error-type bt-has-frames))))))
```

**Property Test Strategy**:

1. **Persistence Across Queries**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")

   ;; Query 100 times
   (let ((results (loop repeat 100
                       collect (describe-last-error))))
     ;; All should be identical
     (apply #'equal results))
   ```

2. **Preservation Across Time**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (sleep 1)
   (let ((error-after-delay (describe-last-error)))
     ;; Still available after delay
     (error-available-p error-after-delay))
   ```

3. **Successful Evaluation Clears**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (assert (error-available-p (describe-last-error)))

   (evaluate-lisp "(+ 1 2)")
   (assert (not (error-available-p (describe-last-error))))
   ```

4. **New Error Replaces Old**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (let ((error1-type (extract-type (describe-last-error))))
     (assert (string= error1-type "DIVISION-BY-ZERO"))

     (evaluate-lisp "(car 'not-a-cons)")
     (let ((error2-type (extract-type (describe-last-error))))
       (and
         (string= error2-type "TYPE-ERROR")
         (not (string= error2-type error1-type)))))
   ```

5. **Cross-Tool Consistency**:
   ```lisp
   (evaluate-lisp "(defun test () (error \"x\")) (test)")

   (let ((error-info (describe-last-error))
         (backtrace (get-backtrace)))
     (and
       ;; Both tools see error state
       (error-available-p error-info)
       (> (length backtrace) 0)

       ;; Query again - still consistent
       (error-available-p (describe-last-error))
       (> (length (get-backtrace)) 0)))
   ```

6. **Non-Error Operations Don't Affect State**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (let ((initial-error (describe-last-error)))

     ;; Other tool calls
     (list-definitions)
     (describe-symbol "car")

     ;; Error state unchanged
     (equal (describe-last-error) initial-error))
   ```

7. **Session Reset Clears Error**:
   ```lisp
   (evaluate-lisp "(/ 1 0)")
   (assert (error-available-p (describe-last-error)))

   (reset-session)
   (assert (not (error-available-p (describe-last-error))))
   ```

**Edge Cases**:

- **Multiple errors in quick succession**: Latest wins
- **Errors during error inspection**: Previous error preserved
- **Compilation vs runtime errors**: Both preserved
- **Reader errors**: Preserved like other errors
- **Memory pressure**: Error state not garbage collected
- **Long-running session**: Error persists indefinitely

**Implementation Requirements**:

```lisp
;; Global error state (per session)
(defvar *last-error-condition* nil)
(defvar *last-error-backtrace* nil)
(defvar *last-error-timestamp* nil)

;; Capture on error
(handler-bind
  ((error
    (lambda (c)
      (setf *last-error-condition* c
            *last-error-backtrace* (capture-backtrace)
            *last-error-timestamp* (get-universal-time)))))
  (eval code))

;; Clear on success
(defun clear-error-state ()
  (setf *last-error-condition* nil
        *last-error-backtrace* nil
        *last-error-timestamp* nil))

;; Preserve during queries
(defun describe-last-error ()
  ;; Read-only access - never modifies state
  (if *last-error-condition*
      (format-error-details *last-error-condition*)
      "No error available"))
```

**Shrinking**: Find minimal operation sequence that loses error state

## Related Properties

- **error-capture-completeness**: Defines what must be preserved
- **backtrace-accuracy**: Backtrace details must not degrade
- **restart-information-completeness**: Restart list must persist
