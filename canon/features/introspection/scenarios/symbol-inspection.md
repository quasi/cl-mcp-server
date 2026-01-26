---
type: scenario
name: symbol-inspection
feature: introspection
version: 0.1.0
---

# Symbol Inspection Scenarios

Scenarios for querying detailed information about Lisp symbols.

## Scenario: Inspect a Standard Function

**Given** the CL package is available
**When** I call describe-symbol with name="mapcar" and package="CL"
**Then** the response includes:
  - Type: FUNCTION
  - Arglist: (FUNCTION LIST &REST MORE-LISTS)
  - Documentation about applying function to list elements
  - Source location (SYS:SRC;CODE;LIST.LISP or similar)

## Scenario: Inspect a Macro

**Given** the CL package is available
**When** I call describe-symbol with name="defun" and package="CL"
**Then** the response includes:
  - Type: MACRO
  - Arglist showing the macro lambda list
  - Documentation about defining functions

## Scenario: Inspect a Special Variable

**Given** the CL package is available
**When** I call describe-symbol with name="*print-base*" and package="CL"
**Then** the response includes:
  - Type: VARIABLE
  - Value: 10 (the current value)
  - Documentation about output base for rationals

## Scenario: Inspect a Class

**Given** the CL package is available
**When** I call describe-symbol with name="standard-class" and package="CL"
**Then** the response includes:
  - Type: CLASS
  - Information about the metaclass

## Scenario: Inspect a Generic Function

**Given** the CL package is available
**When** I call describe-symbol with name="print-object" and package="CL"
**Then** the response includes:
  - Type: GENERIC-FUNCTION
  - Arglist: (OBJECT STREAM)

## Scenario: Symbol Not Found

**Given** CL-USER package exists
**When** I call describe-symbol with name="nonexistent-symbol-xyz"
**Then** the response indicates "Symbol NONEXISTENT-SYMBOL-XYZ not found in package CL-USER"

## Scenario: Package Not Found

**When** I call describe-symbol with name="foo" and package="NONEXISTENT-PKG"
**Then** the response indicates "Package NONEXISTENT-PKG not found"

## Scenario: Symbol in User Package

**Given** I have defined a function `my-test-fn` in CL-USER
**When** I call describe-symbol with name="my-test-fn"
**Then** the response includes:
  - The function's type
  - Its arglist
  - Source location pointing to the REPL or eval location
