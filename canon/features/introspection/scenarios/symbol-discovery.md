---
type: scenario
name: symbol-discovery
feature: introspection
version: 0.1.0
---

# Symbol Discovery Scenarios

Scenarios for finding symbols using apropos-search.

## Scenario: Basic Pattern Search

**When** I call apropos-search with pattern="map"
**Then** the response includes multiple symbols:
  - COMMON-LISP::MAP
  - COMMON-LISP::MAPCAR
  - COMMON-LISP::MAPHASH
  - And other map-related symbols

## Scenario: Package-Scoped Search

**Given** the CL-MCP-SERVER.EVALUATOR package exists
**When** I call apropos-search with pattern="eval" and package="CL-MCP-SERVER.EVALUATOR"
**Then** the response only includes symbols from that package
  - Such as CL-MCP-SERVER.EVALUATOR::EVALUATE-CODE

## Scenario: Type-Filtered Search (Functions Only)

**When** I call apropos-search with pattern="print" and type="function"
**Then** all results have type [FUNCTION]
  - COMMON-LISP::PRINT
  - COMMON-LISP::PRINC
  - COMMON-LISP::PRINT-OBJECT excluded (it's a generic-function)

## Scenario: Type-Filtered Search (Macros Only)

**When** I call apropos-search with pattern="def" and type="macro"
**Then** all results have type [MACRO]
  - COMMON-LISP::DEFUN
  - COMMON-LISP::DEFMACRO
  - COMMON-LISP::DEFCLASS
  - Regular functions excluded

## Scenario: Type-Filtered Search (Variables Only)

**When** I call apropos-search with pattern="print" and type="variable"
**Then** all results have type [VARIABLE]
  - COMMON-LISP::*PRINT-BASE*
  - COMMON-LISP::*PRINT-LENGTH*
  - Functions excluded

## Scenario: Type-Filtered Search (Classes Only)

**When** I call apropos-search with pattern="stream" and type="class"
**Then** all results have type [CLASS]
  - COMMON-LISP::STREAM
  - COMMON-LISP::FILE-STREAM
  - Functions excluded

## Scenario: Type-Filtered Search (Generic Functions Only)

**When** I call apropos-search with pattern="print" and type="generic-function"
**Then** all results have type [GENERIC-FUNCTION]
  - COMMON-LISP::PRINT-OBJECT
  - Regular functions excluded

## Scenario: No Results Found

**When** I call apropos-search with pattern="xyznonexistent123"
**Then** the response indicates "Found 0 symbols matching 'xyznonexistent123'"

## Scenario: Case Insensitivity

**When** I call apropos-search with pattern="MapCar"
**Then** the response includes COMMON-LISP::MAPCAR
  - Pattern matching is case-insensitive

## Scenario: Package Not Found

**When** I call apropos-search with pattern="test" and package="NONEXISTENT-PKG"
**Then** the response indicates "Package NONEXISTENT-PKG not found"

## Scenario: Combined Package and Type Filter

**Given** the CL package contains both functions and macros
**When** I call apropos-search with pattern="def" and package="CL" and type="macro"
**Then** only macros from CL package are returned
