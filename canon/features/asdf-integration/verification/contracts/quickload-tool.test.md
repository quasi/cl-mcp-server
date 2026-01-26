---
type: verification
name: quickload-contract-test
source: contracts/quickload-tool.md
level: contract
tags:
  - asdf-integration
  - quicklisp
---

# Contract Test: Quickload Tool

## Purpose

Verify the `quickload` tool correctly loads ASDF systems via Quicklisp, handling dependencies and providing clear feedback.

## Prerequisites

- Initialized MCP server with ASDF integration tools
- Quicklisp installed and available
- Network access for downloading systems

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)
```

## Test Cases

### Load Common System

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "alexandria"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; System loaded
    (assert (search "ALEXANDRIA" content :test #'char-equal))
    (assert (or (search "Loaded" content :test #'char-equal)
                (search "loaded" content)))

    ;; Version info (may vary)
    (assert (search "version" content :test #'char-equal))))
```

### Already Loaded System

**Setup**: Load alexandria first
```lisp
(call-tool *server* "quickload" '(("system" . "alexandria")))
```

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "alexandria"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Indicates already loaded
    (assert (or (search "already loaded" content :test #'char-equal)
                (search "Loaded" content :test #'char-equal)))))
```

### System with Dependencies

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "drakma",
    "verbose": false
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; System name
    (assert (search "DRAKMA" content :test #'char-equal))

    ;; Dependencies section
    (assert (or (search "Dependencies" content :test #'char-equal)
                (search "dependency" content :test #'char-equal)
                (search "loaded" content)))

    ;; Some known dependencies
    (assert (or (search "USOCKET" content :test #'char-equal)
                (search "FLEXI-STREAMS" content :test #'char-equal)
                (search "dependencies" content :test #'char-equal)))))
```

### Verbose Mode

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "cl-ppcre",
    "verbose": true
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; More detailed output
    (assert (> (length content) 100))  ; Verbose should be longer

    ;; May show compilation info
    (assert (or (search "compil" content :test #'char-equal)
                (search "Loading" content :test #'char-equal)
                (search "loaded" content)))))
```

### System Not Found

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "completely-nonexistent-system-12345"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  ;; Should indicate system not found
  (assert (or (error-response-p response)
              (search "not found" (result-content response) :test #'char-equal)
              (search "System" (result-content response)))))
```

### Silent Mode (Default)

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "babel"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Concise output
    (assert (search "BABEL" content :test #'char-equal))

    ;; Shouldn't have excessive compilation details
    (assert (< (length content) 5000))))  ; Reasonable size
```

### Load Time Reporting

**Input**:
```json
{
  "name": "quickload",
  "arguments": {
    "system": "trivial-gray-streams"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "quickload" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; System loaded
    (assert (search "TRIVIAL-GRAY-STREAMS" content :test #'char-equal))

    ;; May report timing
    (assert (or (search "time" content :test #'char-equal)
                (search "second" content :test #'char-equal)
                (search "Loaded" content)))))
```

### Quicklisp Not Available

**Test**: (Only if Quicklisp can be disabled for testing)
```lisp
;; This test may not be practical in normal env
;; where Quicklisp is always available
```

**Expected**:
```lisp
;; If Quicklisp not available, should error clearly
;; (assert (search "Quicklisp" (error-message response) :test #'char-equal))
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Notes

- Quickload may download systems from internet
- First load compiles, subsequent loads use cached FASLs
- Dependencies automatically resolved and loaded
- Verbose mode shows compilation output
- System not found should produce clear message
- Already-loaded systems handled gracefully
