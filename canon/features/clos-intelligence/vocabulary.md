# CLOS Intelligence Vocabulary

Terms specific to CLOS class and method introspection.

---

## CLOS

**CLOS** (Common Lisp Object System) is the object-oriented programming
facility in Common Lisp, providing:

- Classes and instances
- Multiple inheritance
- Generic functions and methods
- Method combinations
- Metaclass protocol (MOP)

---

## Class

A **Class** defines the structure and behavior of objects:

```lisp
(defclass person ()
  ((name :accessor person-name :initarg :name)
   (age :accessor person-age :initarg :age)))
```

Classes have:
- Slots (instance variables)
- Superclasses (parents in inheritance)
- Subclasses (children in inheritance)
- A metaclass (usually `standard-class`)

---

## Slot

A **Slot** is an instance variable in a CLOS class:

| Slot Property | Description |
|---------------|-------------|
| Name | Slot identifier |
| Type | Type constraint (if specified) |
| Initarg | Keyword for initialization |
| Initform | Default value expression |
| Accessor | Combined reader/writer function |
| Reader | Read-only accessor |
| Writer | Write-only accessor |
| Allocation | `:instance` or `:class` |

---

## Generic Function

A **Generic Function** dispatches to methods based on argument types:

```lisp
(defgeneric draw (shape)
  (:documentation "Draw a shape"))

(defmethod draw ((s circle)) ...)
(defmethod draw ((s square)) ...)
```

The generic function:
- Defines the interface
- Holds all methods
- Dispatches to appropriate method

---

## Method

A **Method** is an implementation of a generic function specialized on
argument types:

```lisp
(defmethod compute ((x integer) (y integer))
  (+ x y))

(defmethod compute ((x float) (y float))
  (* x y))
```

Methods have:
- **Qualifiers**: `:before`, `:after`, `:around`, or none (primary)
- **Specializers**: Types for each required parameter
- **Body**: Implementation code

---

## Method Specializer

A **Method Specializer** constrains method applicability:

| Specializer Type | Example | Meaning |
|------------------|---------|---------|
| Class | `(defmethod f ((x integer)))` | x must be integer |
| EQL | `(defmethod f ((x (eql 'foo))))` | x must be symbol FOO |
| Class-based | `(defmethod f ((x list)))` | x must be a list |

---

## Method Combination

**Method Combination** controls how methods with qualifiers combine:

- **Standard**: Primary with optional before/after/around
- **+**: Adds results of all primary methods
- **and**: ANDs results of all primary methods
- **or**: ORs results (short-circuits)
- **list**: Collects results in list
- Custom combinations via `define-method-combination`

---

## Metaclass

A **Metaclass** is the class of a class, controlling class behavior:

- `standard-class`: Normal CLOS classes
- `built-in-class`: System types (integer, cons, etc.)
- `structure-class`: defstruct types
- `funcallable-standard-class`: Classes whose instances are callable

---

## Class Precedence List (CPL)

The **Class Precedence List** is the linearized inheritance chain:

```lisp
(defclass a () ())
(defclass b (a) ())
(defclass c (a) ())
(defclass d (b c) ())

;; CPL for D: (D B C A STANDARD-OBJECT T)
```

Used for method selection and slot lookup.

---

## MOP (Metaobject Protocol)

The **Metaobject Protocol** is the reflective API for CLOS:

Functions like:
- `class-slots` - Get slot definitions
- `class-direct-superclasses` - Get immediate parents
- `class-precedence-list` - Get linearized inheritance
- `specializer-direct-methods` - Get methods on a class

Requires `(require :sb-clos)` in SBCL.

---

## Accessor Function

An **Accessor Function** is a generated reader/writer for a slot:

```lisp
(defclass person ()
  ((name :accessor person-name)))  ; Creates (setf person-name) too

(let ((p (make-instance 'person)))
  (setf (person-name p) "Alice")
  (person-name p))  ; → "Alice"
```

---

## Direct vs Inherited

CLOS distinguishes:

- **Direct**: Defined in the class itself
  - `class-direct-slots`
  - `class-direct-superclasses`

- **Inherited**: Including from superclasses
  - `class-slots` (all slots)
  - `class-precedence-list` (all ancestors)

---

## Effective Slot

An **Effective Slot** is the computed slot definition after combining
direct slot definitions from the class and its superclasses.

If multiple classes define the same slot, the effective slot merges their
properties according to CLOS rules.
