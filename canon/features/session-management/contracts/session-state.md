---
type: contract
name: session-state
version: 0.1.0
---

# Session State Contract

Defines what state persists across evaluations and how to manage it.

## Persistent State

The following persist between `evaluate-lisp` calls:

### Definitions

| Type | Example | Persists |
|------|---------|----------|
| Functions | `(defun foo ...)` | Yes |
| Macros | `(defmacro bar ...)` | Yes |
| Classes | `(defclass baz ...)` | Yes |
| Structures | `(defstruct qux ...)` | Yes |
| Methods | `(defmethod ...)` | Yes |
| Generic Functions | `(defgeneric ...)` | Yes |

### Variables

| Type | Example | Persists |
|------|---------|----------|
| Special Variables | `(defvar *x* ...)` | Yes |
| Parameters | `(defparameter *y* ...)` | Yes |
| Constants | `(defconstant +z+ ...)` | Yes |
| Setf'd Symbols | `(setf foo 1)` | Yes |

### Package State

| State | Description | Persists |
|-------|-------------|----------|
| Current Package | Value of `*package*` | Yes |
| Created Packages | `(defpackage ...)` | Yes |
| Imported Symbols | `(use-package ...)` | Yes |

### Loaded Systems

| Type | Description | Persists |
|------|-------------|----------|
| Quicklisp Systems | `(ql:quickload ...)` | Yes |
| ASDF Systems | `(asdf:load-system ...)` | Yes |

## Non-Persistent State

The following reset between calls:

| State | Description |
|-------|-------------|
| Let Bindings | Local variables from `let`/`let*` |
| Dynamic Bindings | From `let` with special vars |
| Catch Tags | Active `catch` blocks |
| Restart Bindings | From `restart-case` |
| Handler Bindings | From `handler-bind`/`handler-case` |

## Tools for State Management

### list-definitions

Returns current session definitions:

**Request**:
```json
{
  "name": "list-definitions",
  "arguments": {"type": "all"}
}
```

**Response Format**:
```
[Functions]
- SQUARE (X)
- FACTORIAL (N)

[Variables]
- *COUNTER* = 0
- *DEBUG-MODE* = NIL

[Macros]
- WITH-TIMING (&BODY BODY)

[Classes]
- POINT

[Loaded Systems]
- ALEXANDRIA
- CL-PPCRE
```

### reset-session

Clears all session state:

**Request**:
```json
{
  "name": "reset-session",
  "arguments": {}
}
```

**Response**:
```
Session reset. All definitions cleared.
Current package: CL-USER
```

**What Reset Does**:
1. Unintern all user-defined symbols in CL-USER
2. Reset `*package*` to CL-USER
3. Clear any user-created packages
4. Note: Loaded systems remain (they're part of the Lisp image)

### load-system

Loads an ASDF/Quicklisp system:

**Request**:
```json
{
  "name": "load-system",
  "arguments": {"system": "alexandria"}
}
```

**Response (success)**:
```
Loading system: alexandria
; Loading "alexandria"
Loaded: alexandria
```

**Response (failure)**:
```
[ERROR] QUICKLISP-CLIENT:SYSTEM-NOT-FOUND
System "nonexistent" not found in any known distribution.
```

## Package Context

### Default Package

New sessions start in `CL-USER` package.

### Changing Packages

**Via evaluate-lisp parameter**:
```json
{
  "name": "evaluate-lisp",
  "arguments": {
    "code": "(some-package:function)",
    "package": "SOME-PACKAGE"
  }
}
```

**Via in-band**:
```json
{
  "name": "evaluate-lisp",
  "arguments": {
    "code": "(in-package :my-package)"
  }
}
```

The package change persists for subsequent evaluations.

### Package Errors

Invalid package name:
```
[ERROR] PACKAGE-ERROR
The name "NONEXISTENT" does not designate any package.
```

## State Isolation

Each MCP server process has its own isolated state:
- Multiple clients connecting to different server processes don't share state
- A single client's session is fully isolated in its server process

## Implementation Notes

### Tracking Definitions

Use SBCL's package iteration:
```lisp
(do-symbols (sym (find-package :cl-user))
  (when (and (eq (symbol-package sym) (find-package :cl-user))
             (fboundp sym))
    (collect sym)))
```

### Reset Implementation

```lisp
(defun reset-session ()
  ;; Unintern all user symbols
  (do-symbols (sym (find-package :cl-user))
    (when (eq (symbol-package sym) (find-package :cl-user))
      (unintern sym :cl-user)))
  ;; Reset to CL-USER
  (setf *package* (find-package :cl-user))
  :reset-complete)
```

## Error Response

### list-definitions Errors

This tool has no expected error conditions. It always returns the current definitions.

### reset-session Errors

This tool has no expected error conditions. It always completes successfully.

### load-system Errors

When loading fails:

```json
{
  "content": [
    {
      "type": "text",
      "text": "[ERROR] {CONDITION-TYPE}\n{error message}"
    }
  ],
  "isError": true
}
```

### Possible Errors

| Condition | When | Response |
|-----------|------|----------|
| `ql:system-not-found` | System not in Quicklisp | "System X not found" |
| `asdf:missing-component` | System file not found | "Component X not found" |
| `asdf:compile-error` | Compilation failed | Compilation error details |
| `package-error` | Package conflict | Package error message |

### System Not Found

Input:
```json
{"name": "load-system", "arguments": {"system": "nonexistent"}}
```

Response:
```
[ERROR] QUICKLISP-CLIENT:SYSTEM-NOT-FOUND
System "nonexistent" not found in any known distribution.
```
