---
type: contract
name: class-info-tool
version: 0.1.0
---

# Class-Info Tool Contract

Get comprehensive information about a CLOS class including its slots,
superclasses, subclasses, and metaclass.

## Tool Definition

```json
{
  "name": "class-info",
  "description": "Get complete information about a CLOS class including its slots, superclasses, subclasses, and metaclass. Works with any class in the running image.",
  "inputSchema": {
    "type": "object",
    "required": ["class"],
    "properties": {
      "class": {
        "type": "string",
        "description": "Class name to inspect"
      },
      "package": {
        "type": "string",
        "description": "Package name (defaults to CL-USER)"
      }
    }
  }
}
```

## Input Processing

### Class Resolution

1. Find package (default `CL-USER`)
2. Find symbol in package
3. Get class via `find-class`
4. Return error if class not found

### Package Handling

| Scenario | Behavior |
|----------|----------|
| Package not specified | Use `CL-USER` |
| Package not found | Return error |
| Symbol not found | Return error |
| Symbol not a class | Return "X is not a class" |

## Output Format

### Success Response

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

### Information Included

| Section | Content |
|---------|---------|
| Class name | Symbol and package |
| Metaclass | Usually `STANDARD-CLASS` |
| Direct superclasses | Immediate parents |
| Direct subclasses | Known children |
| CPL | Full inheritance chain |
| Direct slots | Slots defined in this class |
| Slot details | Type, initarg, initform, accessors |

### Slot Information

For each slot:

- **Name**: Slot identifier
- **Type**: Type constraint (if specified)
- **Allocation**: `:instance` (default) or `:class`
- **Initarg**: Keyword for `make-instance`
- **Initform**: Default value expression
- **Accessor**: Combined reader/writer
- **Reader**: Read-only accessor
- **Writer**: Write-only accessor (shown as `(SETF ...)`)

## Class Lookup

### MOP Functions Used

```lisp
(find-class 'person)                    ; Get class object
(class-name class)                      ; Get name
(class-of class)                        ; Get metaclass
(class-direct-superclasses class)       ; Immediate parents
(class-direct-subclasses class)         ; Known children
(class-precedence-list class)           ; Full CPL
(class-direct-slots class)              ; Direct slot definitions
(class-slots class)                     ; All slots (including inherited)
```

### Slot Details

```lisp
(slot-definition-name slot)             ; Slot name
(slot-definition-type slot)             ; Type constraint
(slot-definition-allocation slot)       ; :instance or :class
(slot-definition-initargs slot)         ; List of initargs
(slot-definition-initform slot)         ; Default value form
(slot-definition-readers slot)          ; Reader functions
(slot-definition-writers slot)          ; Writer functions
```

## Examples

### Simple Class

Input:
```json
{
  "class": "person",
  "package": "MY-APP"
}
```

Output:
```
Class: PERSON
  Metaclass: STANDARD-CLASS
  Package: MY-APP

Direct Superclasses:
  - STANDARD-OBJECT

Direct Subclasses: (none)

Class Precedence List:
  PERSON → STANDARD-OBJECT → T

Direct Slots (2):
  NAME
    Initarg: :NAME
    Accessor: PERSON-NAME

  AGE
    Initarg: :AGE
    Initform: 0
    Accessor: PERSON-AGE
```

### Class with Multiple Inheritance

Input:
```json
{
  "class": "flying-car",
  "package": "VEHICLES"
}
```

Output:
```
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

### Built-in Class

Input:
```json
{
  "class": "integer",
  "package": "CL"
}
```

Output:
```
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

### Class Not Found

Input:
```json
{
  "class": "nonexistent-class"
}
```

Output:
```
Class NONEXISTENT-CLASS not found in package CL-USER
```

## Implementation Notes

### MOP Requirements

Requires SBCL's MOP (Metaobject Protocol):

```lisp
(require :sb-clos)  ; or #+sbcl (require :sb-mop)
```

Use package `sb-mop` for portability or `sb-pcl` for SBCL internals.

### Slot Accessor Detection

Accessors, readers, and writers are functions. Check if they exist:

```lisp
(when (fboundp 'person-name)
  (format t "Accessor: PERSON-NAME~%"))
```

### Subclass Discovery

`class-direct-subclasses` only shows classes already loaded in the image.
Unloaded subclasses won't appear.

## Verification Strategy

Tests should verify:

1. **Class found**: Standard classes return info
2. **Superclasses**: Direct and CPL correct
3. **Subclasses**: Known subclasses listed
4. **Slots**: All direct slots with details
5. **Built-in classes**: INTEGER, CONS, etc. work
6. **Multiple inheritance**: CPL linearization correct
7. **Not a class**: Appropriate error for non-classes
8. **Metaclass**: Correctly identified (standard, built-in, etc.)
