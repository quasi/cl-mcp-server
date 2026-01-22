# Session Management Vocabulary

Terms specific to session state tracking.

---

## Binding

A **Binding** associates a symbol with a value in the current environment.
Bindings created via `defvar`, `defparameter`, `setf`, or `let` persist
differently based on their scope.

---

## Definition

A **Definition** is a named construct created via `defun`, `defmacro`,
`defclass`, `defstruct`, etc. Definitions persist in the session and
are available to subsequent evaluations.

---

## Loaded System

A **Loaded System** is an ASDF system that has been loaded via `ql:quickload`
or `asdf:load-system`. The session tracks which systems are loaded.

---

## Current Package

The **Current Package** (the value of `*package*`) determines the namespace
for symbol resolution. Users may switch packages during a session.
