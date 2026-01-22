# How to Load Quicklisp Systems

**Problem**: You need to use a Common Lisp library available through Quicklisp.

Quicklisp is Common Lisp's package manager. This guide shows you how to load libraries within a CL-MCP-Server session.

## Prerequisites

- CL-MCP-Server is running with Quicklisp installed (included in standard setup)
- Basic familiarity with Common Lisp libraries

## Solution: Using ql:quickload

Load libraries using `(ql:quickload :library-name)`:

```lisp
;; Load a single library
(ql:quickload :alexandria)

;; Load multiple libraries at once
(ql:quickload '(:alexandria :cl-ppcre :drakma))
```

**Example conversation**:

```
User: Load the alexandria library
Claude: (evaluating) (ql:quickload :alexandria)
        ; Loading "alexandria"
        .................................................
        [package alexandria]
        => (:ALEXANDRIA)

User: Now use it to create a hash table
Claude: (evaluating)
        (alexandria:alist-hash-table
          '((a . 1) (b . 2) (c . 3)))
        => #<HASH-TABLE :TEST EQL :COUNT 3 {100456A123}>
```

## Common Libraries

Here are some frequently used Quicklisp libraries:

| Library | Description | Use Case |
|---------|-------------|----------|
| `alexandria` | Utility functions | General-purpose helpers |
| `cl-ppcre` | Regular expressions | Pattern matching |
| `drakma` | HTTP client | Web requests |
| `local-time` | Date/time handling | Time operations |
| `parse-number` | Number parsing | String to number conversion |
| `cl-json` | JSON parser | JSON handling |
| `str` | String utilities | String manipulation |
| `iterate` | Iteration macro | Advanced loops |
| `serapeum` | Utility library | Modern Lisp idioms |

## Loading Systems

### Load and Use Immediately

```lisp
;; Load and immediately use
(progn
  (ql:quickload :str)
  (str:concat "Hello" " " "World"))
;; => "Hello World"
```

### Check if Already Loaded

```lisp
;; Check if a system is loaded
(asdf:component-loaded-p (asdf:find-system :alexandria))

;; Load only if not already loaded
(unless (asdf:component-loaded-p (asdf:find-system :alexandria))
  (ql:quickload :alexandria))
```

## Persistence

**Important**: Loaded systems persist for the entire session.

```
User: Load alexandria
Claude: (evaluating) (ql:quickload :alexandria)
        => (:ALEXANDRIA)

User: [later in session] Use alexandria:if-let
Claude: (evaluating)
        (alexandria:if-let ((x (+ 1 2)))
          (* x 2))
        => 6
        ;; No need to reload - it's still loaded!
```

## Finding Libraries

### Search Quicklisp

You can search for available systems:

```lisp
;; Search for systems matching a pattern (returns many results)
(ql:system-apropos "json")

;; List all available systems (very long output)
(length (ql:system-list))
```

### Browse Online

- [Quicklisp Library List](https://www.quicklisp.org/beta/releases.html) - Official list
- [Quickdocs](http://quickdocs.org/) - Searchable documentation
- [Awesome CL](https://github.com/CodyReichert/awesome-cl) - Curated list

## Common Errors

### System Not Found

**Error**:
```
[ERROR] QUICKLISP-CLIENT:SYSTEM-NOT-FOUND
System "nonexistent-lib" not found.
```

**Fixes**:

1. Check the spelling:
```lisp
;; Wrong
(ql:quickload :alex)

;; Correct
(ql:quickload :alexandria)
```

2. Update Quicklisp to get newer libraries:
```lisp
(ql:update-client)
(ql:update-all-dists)
```

3. Search for the correct name:
```lisp
(ql:system-apropos "search-term")
```

### Compilation Errors During Load

**Error**:
```
[ERROR] during system load
Compilation failed in system "some-lib"
```

**This usually means**:
- The library has unmet dependencies (rare with Quicklisp)
- The library is incompatible with your Lisp implementation
- The library is outdated

**Fix**: Try a different library or check the library's documentation for requirements.

## Using ASDF Directly

For systems already present but not loaded:

```lisp
;; Load via ASDF (for local systems)
(asdf:load-system :my-local-system)
```

**Difference**:
- `ql:quickload` - Downloads from internet if needed, then loads
- `asdf:load-system` - Only loads systems already present locally

## Verification

Check what systems are loaded:

```lisp
;; List loaded systems
(remove-if-not #'asdf:component-loaded-p
               (asdf:registered-systems))
```

## Advanced: Installing Specific Versions

Quicklisp uses distributions (snapshots of all libraries at a point in time):

```lisp
;; See available dist versions
(ql-dist:available-versions (ql-dist:dist "quicklisp"))

;; Install a specific dist version
(ql-dist:install-dist
  "http://beta.quicklisp.org/dist/quicklisp/2023-10-21/distinfo.txt"
  :replace t)
```

**Note**: This affects the entire Lisp session, not just CL-MCP-Server.

## Example: Building a Web Scraper

```lisp
;; Load dependencies
(ql:quickload '(:drakma :cl-ppcre))

;; Fetch a webpage
(defparameter *html*
  (drakma:http-request "https://example.com"))

;; Extract all links
(cl-ppcre:all-matches-as-strings
  "https?://[^\"<>\\s]+"
  *html*)
```

## See Also

- [Future: load-system tool](../../canon/features/session-management/contracts/session-state.md#load-system) - Planned MCP tool for loading systems
- [Quicklisp Documentation](https://www.quicklisp.org/beta/) - Official Quicklisp docs
- [Session State Contract](../../canon/features/session-management/contracts/session-state.md) - What persists across evaluations
