# Introspection Vocabulary

Terms specific to SBCL introspection and image inspection.

---

## Symbol Type

A **Symbol Type** classifies what a symbol names in the Lisp image:

| Type | Description |
|------|-------------|
| `:function` | Regular function (defun) |
| `:macro` | Macro function |
| `:generic-function` | CLOS generic function |
| `:class` | CLOS class |
| `:variable` | Bound special variable |
| `:symbol` | Unbound symbol (exists but not defined) |

---

## Symbol Information

**Symbol Information** is the comprehensive data about a symbol:

- **name**: Symbol name string
- **package**: Package name (or nil if uninterned)
- **type**: Symbol type classification
- **documentation**: Docstring if available
- **arglist**: Lambda list for functions/macros
- **value**: Printed representation for bound variables
- **source-file**: Definition source file path
- **source-offset**: Character offset in source file

---

## Apropos

**Apropos** is the traditional Lisp operation for finding symbols matching
a pattern. Case-insensitive substring search across symbol names.

---

## Cross-Reference (XREF)

**Cross-Reference** data tracks relationships between code:

- **who-calls**: Functions that call a given function
- **who-references**: Code that reads a given variable
- **who-binds**: Code that binds a given variable
- **who-sets**: Code that writes a given variable

SBCL maintains this database via `sb-introspect`.

---

## sb-introspect

**sb-introspect** is SBCL's built-in introspection module providing:

- `function-lambda-list` - Get function argument list
- `find-definition-sources-by-name` - Locate source definitions
- `who-calls` - Find callers of a function
- `who-references` - Find references to a variable

Must be loaded with `(require :sb-introspect)`.

---

## Macro Expansion

**Macro Expansion** transforms macro calls into their expanded code:

- **macroexpand-1**: One step of expansion (just the outermost macro)
- **macroexpand**: Full expansion (recursive until no macros remain)

Useful for understanding what macros like `defun`, `loop`, or `with-*`
actually generate.

---

## Definition Source

A **Definition Source** (from `sb-introspect:definition-source`) contains:

- **pathname**: File path where defined
- **character-offset**: Position in file
- **form-number**: Which toplevel form
- **plist**: Additional metadata

Used to locate where symbols are defined in source code.
