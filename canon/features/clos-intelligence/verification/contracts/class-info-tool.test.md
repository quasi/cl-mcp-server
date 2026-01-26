---
type: verification
name: class-info-contract-test
source: contracts/class-info-tool.md
level: contract
tags:
  - clos-intelligence
  - introspection
---

# Contract Test: Class-Info Tool

## Purpose

Verify the `class-info` tool correctly introspects CLOS classes including slots, superclasses, subclasses, and metaclasses.

## Prerequisites

- Initialized MCP server with CLOS intelligence tools
- Test classes defined

## Setup

```lisp
(defvar *server* (make-test-mcp-server))
(initialize-server *server*)

;; Define test classes
(defclass person ()
  ((name :initarg :name :accessor person-name :type string)
   (age :initarg :age :accessor person-age :initform 0 :type integer)))

(defclass employee (person)
  ((employee-id :initarg :id :accessor employee-id :type integer)
   (salary :initarg :salary :reader employee-salary :type number)))
```

## Test Cases

### Simple Class Info

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "person",
    "package": "CL-USER"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Class name
    (assert (search "PERSON" content :test #'char-equal))

    ;; Metaclass
    (assert (search "STANDARD-CLASS" content :test #'char-equal))

    ;; Superclass
    (assert (search "STANDARD-OBJECT" content :test #'char-equal))

    ;; Direct slots
    (assert (search "NAME" content :test #'char-equal))
    (assert (search "AGE" content :test #'char-equal))

    ;; Slot details
    (assert (search ":NAME" content :test #'char-equal))  ; initarg
    (assert (search "PERSON-NAME" content :test #'char-equal))  ; accessor
    (assert (search "PERSON-AGE" content :test #'char-equal))))
```

### Class with Inheritance

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "employee",
    "package": "CL-USER"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Direct superclass
    (assert (search "PERSON" content :test #'char-equal))

    ;; CPL includes both
    (assert (search "EMPLOYEE" content :test #'char-equal))
    (assert (search "Class Precedence List" content :test #'char-equal))

    ;; Direct slots (not inherited)
    (assert (search "EMPLOYEE-ID" content :test #'char-equal))
    (assert (search "SALARY" content :test #'char-equal))

    ;; Reader (not accessor)
    (assert (search "EMPLOYEE-SALARY" content :test #'char-equal))
    (assert (search "reader" content :test #'char-equal))))
```

### Built-in Class

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "integer",
    "package": "CL"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Built-in metaclass
    (assert (search "BUILT-IN-CLASS" content :test #'char-equal))

    ;; Superclass
    (assert (search "RATIONAL" content :test #'char-equal))

    ;; Subclasses
    (assert (or (search "FIXNUM" content :test #'char-equal)
                (search "BIGNUM" content :test #'char-equal)))

    ;; No slots (built-in)
    (assert (or (search "no slots" content :test #'char-equal)
                (search "Direct Slots: (none)" content :test #'char-equal)))))
```

### Class Not Found

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "nonexistent-class"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  ;; Should be error or informative message
  (assert (or (error-response-p response)
              (search "not found" (result-content response) :test #'char-equal))))
```

### Invalid Package

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "person",
    "package": "NONEXISTENT-PACKAGE"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (error-response-p response))
  (assert (search "package" (error-message response) :test #'char-equal)))
```

### Multiple Inheritance

**Setup**:
```lisp
(defclass vehicle ()
  ((wheels :initarg :wheels :accessor vehicle-wheels)))

(defclass aircraft ()
  ((wings :initarg :wings :accessor aircraft-wings)))

(defclass flying-car (vehicle aircraft)
  ((vtol :initarg :vtol :accessor flying-car-vtol :initform t)))
```

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "flying-car"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Both superclasses
    (assert (search "VEHICLE" content :test #'char-equal))
    (assert (search "AIRCRAFT" content :test #'char-equal))

    ;; CPL linearization
    (assert (search "Class Precedence List" content :test #'char-equal))

    ;; Own slot
    (assert (search "VTOL" content :test #'char-equal))))
```

### Slot Type Information

**Setup**:
```lisp
(defclass typed-class ()
  ((count :type (integer 0 100))
   (name :type string)
   (data :type list)))
```

**Input**:
```json
{
  "name": "class-info",
  "arguments": {
    "class": "typed-class"
  }
}
```

**Expected**:
```lisp
(let ((response (call-tool *server* "class-info" input)))
  (assert (result-response-p response))

  (let ((content (result-content response)))
    ;; Type constraints mentioned
    (assert (or (search "Type:" content :test #'char-equal)
                (search "INTEGER" content :test #'char-equal)
                (search "STRING" content :test #'char-equal)))))
```

## Teardown

```lisp
(shutdown-test-server *server*)
```

## Notes

- Class introspection must use MOP functions
- Slot details include type, initargs, initforms, accessors
- Built-in classes have BUILT-IN-CLASS metaclass
- Multiple inheritance shows all superclasses and linearized CPL
- Missing classes should produce clear error messages
