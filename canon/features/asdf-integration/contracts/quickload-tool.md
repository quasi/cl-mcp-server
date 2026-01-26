---
type: contract
name: quickload-tool
version: 0.1.0
---

# Quickload Tool Contract

Load an ASDF system via Quicklisp. Automatically downloads the system and
its dependencies if not already installed.

## Tool Definition

```json
{
  "name": "quickload",
  "description": "Load an ASDF system via Quicklisp. Will automatically download the system and its dependencies if not already installed. Safer than load-system for external dependencies.",
  "inputSchema": {
    "type": "object",
    "required": ["system"],
    "properties": {
      "system": {
        "type": "string",
        "description": "Name of the system to load"
      },
      "verbose": {
        "type": "boolean",
        "description": "Show detailed loading output (default: false)"
      }
    }
  }
}
```

## Input Processing

### System Loading

1. Check if Quicklisp is available
2. Call `ql:quickload` with system name
3. Quicklisp downloads if not installed
4. ASDF compiles and loads system
5. Return success or error details

### Verbose Mode

| Mode | Behavior |
|------|----------|
| `false` (default) | Show summary only |
| `true` | Show compilation output, warnings |

## Output Format

### Success Response (Non-verbose)

```
Loaded system: DRAKMA (version 2.0.8)

Dependencies loaded:
  - USOCKET (2.8.3)
  - FLEXI-STREAMS (1.0.19)
  - CHUNGA (1.1.7)
  - CL-BASE64 (3.3.4)
  - PURI (1.5.7)
  - CHIPZ (0.8)
  - CL+SSL (latest)

Load time: 2.34 seconds
Systems compiled: 8
```

### Success Response (Verbose)

```
Loading system: DRAKMA
  → Downloading DRAKMA (2.0.8)...
  → Installing to ~/quicklisp/dists/quicklisp/software/drakma-2.0.8/

Checking dependencies:
  ✓ USOCKET (2.8.3) - already installed
  ✓ FLEXI-STREAMS (1.0.19) - already installed
  ✗ CHUNGA - not found, downloading...
    → Downloaded CHUNGA (1.1.7)

Compiling DRAKMA:
  ; compiling file drakma.lisp
  ; wrote /cache/.../drakma.fasl

Loaded system: DRAKMA (version 2.0.8)
Load time: 2.34 seconds
```

### Download Required

```
System CL-PPCRE not found locally.
Downloading from Quicklisp...

  → Fetching cl-ppcre (2.1.1) from dist/quicklisp/...
  → Installing to ~/quicklisp/dists/quicklisp/software/cl-ppcre-2.1.1/

Downloaded 156 KB
Compiling...
Loaded system: CL-PPCRE (version 2.1.1)
```

### System Not Found

```
System "nonexistent-system" not found in Quicklisp.

Try:
  (ql:system-apropos "search-term")

Or check: https://www.quicklisp.org/beta/
```

### Quicklisp Not Available

```
Error: Quicklisp is not installed or not loaded.

To install Quicklisp:
  1. Download: https://beta.quicklisp.org/quicklisp.lisp
  2. Load: (load "quicklisp.lisp")
  3. Install: (quicklisp-quickstart:install)
```

## Quicklisp Behavior

### System Discovery

Quicklisp searches:
1. Local installation cache
2. Current dist (usually "quicklisp" dated dist)
3. Remote Quicklisp servers

### Download Process

If system not found locally:
1. Fetch system archive from Quicklisp servers
2. Extract to `~/quicklisp/dists/quicklisp/software/`
3. Register with ASDF
4. Proceed to load

### Dependency Resolution

Quicklisp automatically:
- Resolves transitive dependencies
- Downloads missing dependencies
- Loads in correct order

### Compilation

Systems are compiled on first load:
- FASLs stored in `~/.cache/common-lisp/`
- Subsequent loads use cached FASLs
- Faster after first load

## Examples

### Load Common Library

Input:
```json
{
  "system": "alexandria",
  "verbose": false
}
```

Output:
```
Loaded system: ALEXANDRIA (version 1.4)
Dependencies loaded: 0
Load time: 0.12 seconds
```

### Load with Dependencies

Input:
```json
{
  "system": "hunchentoot",
  "verbose": false
}
```

Output:
```
Loaded system: HUNCHENTOOT (version 1.3.0)

Dependencies loaded:
  - BORDEAUX-THREADS (0.9.1)
  - CHUNGA (1.1.7)
  - CL-BASE64 (3.3.4)
  - CL-FAD (0.7.6)
  - CL-PPCRE (2.1.1)
  - FLEXI-STREAMS (1.0.19)
  - CL+SSL (latest)
  - MD5 (2.0.4)
  - RFC2388 (latest)
  - TRIVIAL-BACKTRACE (1.1.0)
  - USOCKET (2.8.3)

Load time: 3.45 seconds
Systems compiled: 12
```

### System Already Loaded

Input:
```json
{
  "system": "alexandria"
}
```

Output:
```
System ALEXANDRIA is already loaded (version 1.4)

(No compilation needed)
```

### System Not in Quicklisp

Input:
```json
{
  "system": "my-private-lib"
}
```

Output:
```
System "my-private-lib" not found in Quicklisp.

This may be a local system. Try:
  (asdf:load-system "my-private-lib")

Or use load-system tool for local systems.
```

## Implementation Notes

### Quicklisp Availability

Check before using:

```lisp
(when (find-package :quicklisp)
  (funcall (find-symbol "QUICKLOAD" :ql) system-name))
```

### Silent Mode Implementation

```lisp
;; Suppress compilation output
(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (ql:quickload system-name :silent t))
```

### Tracking Dependencies

Quicklisp provides:

```lisp
(ql:who-depends-on "system-name")  ; Show dependents
(ql:required-systems "system-name") ; Show dependencies
```

### Version Information

```lisp
(ql-dist:find-system "system-name")
;; Returns system object with version info
```

## Safety Considerations

### Network Access

Quickload may:
- Download from internet
- Execute downloaded code
- Install files to disk

Users should trust Quicklisp and the systems they install.

### Disk Usage

Downloaded systems consume disk space:
- Software: `~/quicklisp/dists/quicklisp/software/`
- FASLs: `~/.cache/common-lisp/`

### Compilation Warnings

Systems may emit warnings during compilation. These are usually benign
but should be shown in verbose mode.

## Verification Strategy

Tests should verify:

1. **Load success**: System loads without error
2. **Dependencies**: All dependencies loaded
3. **Compilation**: FASLs generated
4. **Already loaded**: Handles already-loaded systems
5. **Not found**: Appropriate error for missing systems
6. **Verbose mode**: Shows compilation output
7. **Silent mode**: Suppresses unnecessary output
8. **Network failure**: Graceful handling of connection issues
