# CLOS Intelligence Tools Reference

<!-- Generated from: canon/features/clos-intelligence/contracts/*.md -->

Tools for inspecting and understanding Common Lisp Object System (CLOS) classes.

## Overview

CLOS intelligence tools allow you to explore object-oriented code in Common Lisp:
- Inspect class definitions and inheritance hierarchies
- View slot specifications and accessor functions
- Understand the class precedence list (method resolution order)
- Discover relationships between classes

## class-info

Get comprehensive information about a CLOS class including slots, superclasses, subclasses, and metaclass.

### Parameters

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| class | string | Yes | Class name to inspect |
| package | string | No | Package name (defaults to CL-USER) |

### Output

Shows complete class information:

```
Class: PERSON
  Metaclass: STANDARD-CLASS
  Package: MY-APP

Direct Superclasses:
  - STANDARD-OBJECT

Direct Subclasses:
  - EMPLOYEE
  - CUSTOMER

Class Precedence List:
  PERSON → STANDARD-OBJECT → T

Direct Slots (2):
  NAME
    Type: STRING
    Initarg: :NAME
    Accessor: PERSON-NAME

  AGE
    Type: (INTEGER 0 120)
    Initarg: :AGE
    Initform: 0
    Accessor: PERSON-AGE
    Reader: GET-PERSON-AGE

All Slots (inherited included): 2
```

### Examples

**Inspect a user-defined class:**

```lisp
;; First define a class
(defclass person ()
  ((name :initarg :name
         :accessor person-name
         :type string)
   (age :initarg :age
        :accessor person-age
        :initform 0
        :type (integer 0 120))))
```

```
User: Please use class-info on "person"

Response:
Class: PERSON
  Metaclass: STANDARD-CLASS
  Package: CL-USER

Direct Superclasses:
  - STANDARD-OBJECT

Class Precedence List:
  PERSON → STANDARD-OBJECT → T

Direct Slots (2):
  NAME
    Type: STRING
    Initarg: :NAME
    Accessor: PERSON-NAME

  AGE
    Type: (INTEGER 0 120)
    Initarg: :AGE
    Initform: 0
    Accessor: PERSON-AGE
```

**Inspect a built-in class:**

```
User: Please use class-info on "integer" in package "CL"

Response:
Class: INTEGER
  Metaclass: BUILT-IN-CLASS
  Package: COMMON-LISP

Direct Superclasses:
  - RATIONAL

Direct Subclasses:
  - BIGNUM
  - FIXNUM

Class Precedence List:
  INTEGER → RATIONAL → REAL → NUMBER → T

Direct Slots: (none)
  (Built-in classes typically have no inspectable slots)
```

**Class with multiple inheritance:**

```lisp
(defclass car () ...)
(defclass aircraft () ...)

(defclass flying-car (car aircraft)
  ((vtol-capable :initarg :vtol-capable
                 :accessor flying-car-vtol-capable
                 :initform t
                 :type boolean)))
```

```
User: Please use class-info on "flying-car"

Response:
Class: FLYING-CAR
  Metaclass: STANDARD-CLASS

Direct Superclasses:
  - CAR
  - AIRCRAFT

Class Precedence List:
  FLYING-CAR → CAR → AIRCRAFT → VEHICLE → STANDARD-OBJECT → T

Direct Slots (1):
  VTOL-CAPABLE
    Type: BOOLEAN
    Initarg: :VTOL-CAPABLE
    Initform: T
    Accessor: FLYING-CAR-VTOL-CAPABLE

All Slots (inherited included): 8
  (includes slots from CAR and AIRCRAFT)
```

### Slot Information Provided

For each slot, class-info shows:

| Field | Description |
|-------|-------------|
| Name | Slot identifier |
| Type | Type constraint (if specified) |
| Allocation | `:instance` (per-object) or `:class` (shared) |
| Initarg | Keyword for `make-instance` |
| Initform | Default value expression |
| Accessor | Combined reader/writer function |
| Reader | Read-only accessor |
| Writer | Write-only accessor |

### Understanding Class Precedence List (CPL)

The CPL shows the method resolution order—when a generic function is called, methods are chosen based on this order:

```
FLYING-CAR → CAR → AIRCRAFT → VEHICLE → STANDARD-OBJECT → T
```

This means:
1. Methods specialized on `FLYING-CAR` are tried first
2. Then methods on `CAR`
3. Then `AIRCRAFT`
4. And so on...

### Notes

- Uses SBCL's MOP (Metaobject Protocol) for introspection
- Subclasses list only shows classes already loaded in the image
- Built-in classes (like `INTEGER`, `CONS`) have limited slot information
- For condition classes, use `find-class` to verify it's a class first

### Common Use Cases

**Exploring a library's class hierarchy:**
```
1. Use apropos-search to find class names
2. Use class-info to understand each class
3. Examine the CPL to understand inheritance
```

**Understanding slot accessors:**
```
1. Use class-info to see all slots
2. Note the accessor/reader/writer names
3. Use describe-symbol on accessors for more details
```

**Debugging inheritance issues:**
```
1. Use class-info to see the CPL
2. Verify slot inheritance
3. Check for slot name conflicts
```

---

## See Also

- [describe-symbol](introspection-tools.md#describe-symbol) - Get details on accessor functions
- [apropos-search](introspection-tools.md#apropos-search) - Find classes by name
- [CLOS Tutorial](../tutorials/exploring-clos.md) - Learn CLOS introspection step-by-step
