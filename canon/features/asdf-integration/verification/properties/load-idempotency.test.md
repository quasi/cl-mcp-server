---
type: verification
name: load-idempotency-property-test
source: properties/load-idempotency.md
level: property
tags:
  - property-based
  - asdf
  - idempotency
---

# Property Test: Load Idempotency

## Purpose

Verify that loading a system multiple times is idempotent and doesn't cause errors or duplicate side effects.

## Prerequisites

- Initialized MCP server
- ASDF integration tools available

## Implementation

### Property: Repeated Load Same Result

```lisp
(deftest load-idempotency-repeated-loads ()
  "Loading same system multiple times succeeds"
  (let ((systems '("alexandria" "bordeaux-threads" "split-sequence")))
    (dolist (system systems)
      ;; Load 3 times
      (dotimes (i 3)
        (let ((response (call-tool *test-server* "quickload"
                                  `(("system" . ,system)))))

          ;; Each load should succeed
          (is (result-response-p response)
              "Load ~A of system ~A failed" (1+ i) system)

          ;; Should indicate success or already-loaded
          (let ((content (result-content response)))
            (is (or (search "loaded" content :test #'char-equal)
                    (search "Loaded" content))
                "No success indicator for ~A load ~A" system (1+ i))))))))
```

### Property: System State Consistent

```lisp
(deftest load-idempotency-state-consistent ()
  "Multiple loads don't duplicate definitions"
  (let ((system "alexandria"))
    ;; Load once
    (call-tool *test-server* "quickload" `(("system" . ,system)))

    ;; Check a known function exists
    (let* ((check-code "(fboundp 'alexandria:flatten)")
           (r1 (call-tool *test-server* "evaluate-lisp"
                         `(("code" . ,check-code)))))

      (is (search "T" (result-content r1) :test #'char-equal)
          "Alexandria function not available after first load")

      ;; Load again
      (call-tool *test-server* "quickload" `(("system" . ,system)))

      ;; Function still exists (not undefined/redefined in bad way)
      (let ((r2 (call-tool *test-server* "evaluate-lisp"
                          `(("code" . ,check-code)))))

        (is (search "T" (result-content r2) :test #'char-equal)
            "Alexandria function not available after second load")))))
```

### Property: Load Time Faster on Repeat

```lisp
(deftest load-idempotency-cached-load-faster ()
  "Second load faster due to FASL caching"
  (let ((system "cl-ppcre"))
    ;; First load (may compile)
    (let* ((start1 (get-universal-time))
           (r1 (call-tool *test-server* "quickload" `(("system" . ,system))))
           (time1 (- (get-universal-time) start1)))

      (is (result-response-p r1) "First load failed")

      ;; Second load (uses cached FASLs)
      (let* ((start2 (get-universal-time))
             (r2 (call-tool *test-server* "quickload" `(("system" . ,system))))
             (time2 (- (get-universal-time) start2)))

        (is (result-response-p r2) "Second load failed")

        ;; Second should be same or faster (allow equal for very fast loads)
        (is (<= time2 (+ time1 1))
            "Second load took ~A seconds, first took ~A seconds" time2 time1)))))
```

### Property: No Warning on Redef

```lisp
(deftest load-idempotency-no-redef-warnings ()
  "Repeated loads don't generate redefinition warnings"
  (let ((system "split-sequence"))
    ;; Load twice
    (call-tool *test-server* "quickload" `(("system" . ,system)))

    (let* ((response (call-tool *test-server* "quickload"
                               `(("system" . ,system)
                                 ("verbose" . t))))
           (content (result-content response)))

      ;; Should not have redefinition warnings
      ;; (Some output OK, but not warnings about redefining)
      (is (not (and (search "warning" content :test #'char-equal)
                    (search "redefin" content :test #'char-equal)))
          "Redefinition warnings on second load of ~A" system))))
```

## Configuration

- Examples: Test with 3-5 common systems
- Load each system 2-3 times
- Check state consistency between loads

## Notes

- ASDF maintains system load state to avoid redundant reloading
- Second loads should use cached FASLs (faster)
- No errors or warnings on repeated loads
- System functionality unchanged by reloading
