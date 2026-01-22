# How to Switch Packages

**Problem**: You need to evaluate code in a different package namespace.

Common Lisp uses packages to organize symbols. By default, evaluations happen in the `CL-USER` package, but you may need to work in other packages.

## Prerequisites

- CL-MCP-Server is running and connected to Claude
- Basic understanding of Common Lisp packages

## Solution 1: In-Band Package Switching

Use `(in-package :package-name)` within your code:

```lisp
;; Switch to a different package
(in-package :cl)

;; Now you're in the CL package
;; Subsequent evaluations will use this package
```

**Example conversation**:

```
User: Please evaluate (in-package :cl)
Claude: => #<PACKAGE "COMMON-LISP">

User: Please evaluate *package*
Claude: => #<PACKAGE "COMMON-LISP">
```

**Note**: The package change persists across evaluations.

## Solution 2: Using the Package Parameter

If the `evaluate-lisp` tool supports a package parameter (future enhancement), you can specify the package explicitly:

```json
{
  "name": "evaluate-lisp",
  "arguments": {
    "code": "(some-function)",
    "package": "MY-PACKAGE"
  }
}
```

**Status**: Not yet implemented. Use Solution 1 for now.

## Common Patterns

### Creating and Switching to a New Package

```lisp
;; Step 1: Define the package
(defpackage :my-app
  (:use :cl)
  (:export #:main #:config))

;; Step 2: Switch to it
(in-package :my-app)

;; Step 3: Define symbols in your package
(defun main ()
  (format t "Hello from MY-APP package!~%"))
```

### Temporarily Evaluating in Another Package

```lisp
;; Evaluate in another package without switching permanently
(let ((*package* (find-package :some-package)))
  (read-from-string "some-symbol"))
```

### Using Qualified Symbols

Instead of switching packages, use package prefixes:

```lisp
;; Call a function from another package
(alexandria:if-let ((x (get-value)))
  (process x))

;; Access a symbol from another package
some-package:*global-var*
```

## Common Errors

### Package Does Not Exist

**Error**:
```
[ERROR] PACKAGE-ERROR
The name "NONEXISTENT" does not designate any package.
```

**Fix**: Create the package first with `defpackage` or verify the package name is correct.

```lisp
;; Check available packages
(list-all-packages)

;; Create the package if it doesn't exist
(defpackage :my-package (:use :cl))
(in-package :my-package)
```

### Symbol Not Accessible

**Error**:
```
[ERROR] READER-ERROR
Symbol "PRIVATE-FUNCTION" not found in package MY-PACKAGE.
```

**Fix**: Either use the package that exported the symbol, or use a double colon to access internal symbols:

```lisp
;; For exported symbols (single colon)
my-package:public-function

;; For internal symbols (double colon - use with caution)
my-package::private-function
```

## Verification

Check your current package:

```lisp
;; See current package
*package*

;; See current package name
(package-name *package*)
```

## Returning to CL-USER

To return to the default package:

```lisp
(in-package :cl-user)
```

## See Also

- [Session State Contract](../../canon/features/session-management/contracts/session-state.md) - How package state persists
- [Common Lisp HyperSpec: Packages](http://www.lispworks.com/documentation/HyperSpec/Body/11_.htm) - Complete package documentation
- [Tutorial: First REPL Session](../tutorials/01-first-session.md) - Basic evaluation patterns
