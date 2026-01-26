# ASDF and Quicklisp Tools Reference

<!-- Generated from: canon/features/asdf-integration/contracts/*.md -->

Tools for loading and managing Common Lisp systems and libraries.

## Overview

ASDF (Another System Definition Facility) and Quicklisp integration allows you to:
- Load Common Lisp libraries and systems
- Automatically download missing dependencies
- Manage project dependencies
- Work with the broader Common Lisp ecosystem

## quickload

Load an ASDF system via Quicklisp with automatic dependency resolution and downloading.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| system | string | Yes | Name of the system to load (e.g., "alexandria", "hunchentoot") |
| verbose | boolean | No | Show detailed compilation output (default: false) |

### What It Does

1. Checks if the system is already loaded
2. If not found locally, downloads from Quicklisp
3. Resolves and downloads all dependencies
4. Compiles the system (cached for future loads)
5. Makes the system available in your session

### Examples

**Load a popular library:**

```
User: Please quickload "alexandria"

Response:
Loaded system: ALEXANDRIA (version 1.4)
Dependencies loaded: 0
Load time: 0.12 seconds
```

**Load a library with dependencies:**

```
User: Please quickload "hunchentoot"

Response:
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

**System already loaded:**

```
User: Please quickload "alexandria"

Response:
System ALEXANDRIA is already loaded (version 1.4)

(No compilation needed)
```

**Download required:**

```
User: Please quickload "cl-ppcre"

Response:
System CL-PPCRE not found locally.
Downloading from Quicklisp...

  → Fetching cl-ppcre (2.1.1) from dist/quicklisp/...
  → Installing to ~/quicklisp/dists/quicklisp/software/cl-ppcre-2.1.1/

Downloaded 156 KB
Compiling...
Loaded system: CL-PPCRE (version 2.1.1)
```

**System not in Quicklisp:**

```
User: Please quickload "nonexistent-system"

Response:
System "nonexistent-system" not found in Quicklisp.

Try:
  (ql:system-apropos "search-term")

Or check: https://www.quicklisp.org/beta/
```

### Verbose Mode

Use `verbose: true` to see detailed compilation output:

```json
{"system": "drakma", "verbose": true}
```

Shows:
- Download progress
- Dependency resolution steps
- Compilation messages
- Warnings and notes

### Popular Libraries You Can Load

| Library | Purpose | Example Use |
|---------|---------|-------------|
| alexandria | Utility functions | General Lisp programming |
| cl-ppcre | Regular expressions | String pattern matching |
| dexador | HTTP client | Making HTTP requests |
| jonathan | JSON parsing | API integration |
| local-time | Time handling | Date/time manipulation |
| bordeaux-threads | Threading | Concurrent programming |
| cl-fad | File operations | Portable file manipulation |
| ironclad | Cryptography | Hashing, encryption |
| hunchentoot | Web server | Building web applications |

### How Quicklisp Works

**First Load:**
1. System not found locally → downloads from Quicklisp servers
2. Extracts to `~/quicklisp/dists/quicklisp/software/`
3. Compiles all .lisp files
4. Stores compiled FASLs in `~/.cache/common-lisp/`
5. Loads the system

**Subsequent Loads:**
1. System found locally
2. Uses cached FASLs (much faster)
3. No recompilation needed (unless source changed)

### Troubleshooting

**Quicklisp not installed:**

```
Error: Quicklisp is not installed or not loaded.

To install Quicklisp:
  1. Download: https://beta.quicklisp.org/quicklisp.lisp
  2. Load: (load "quicklisp.lisp")
  3. Install: (quicklisp-quickstart:install)
```

**Network issues:**

If Quicklisp can't download a system, check:
- Internet connection
- Firewall settings
- Proxy configuration (if behind corporate firewall)

**Compilation warnings:**

Systems may emit warnings during compilation. These are usually benign but worth noting. Use `verbose: true` to see them.

### Notes

- Quickload requires internet access for first-time downloads
- Downloaded systems consume disk space in `~/quicklisp/`
- Compiled code cached in `~/.cache/common-lisp/`
- Systems are loaded into the current session (persist until session ends)

### Common Workflows

**Starting a new project:**
```
1. quickload common utilities (alexandria, etc.)
2. quickload project-specific dependencies
3. Begin development
```

**Exploring libraries:**
```
1. Search Quicklisp: https://www.quicklisp.org/beta/
2. quickload to try it out
3. Use describe-symbol and class-info to explore
```

**Dependency management:**
```
1. Note which systems you use
2. Document in your .asd file
3. Users can quickload your system + dependencies
```

---

## See Also

- [evaluate-lisp](evaluate-lisp.md) - Execute Lisp code with loaded systems
- [How to: Load Quicklisp Systems](../how-to/load-quicklisp-systems.md) - Practical guide
- [Quicklisp Documentation](https://www.quicklisp.org/beta/) - Official Quicklisp docs
