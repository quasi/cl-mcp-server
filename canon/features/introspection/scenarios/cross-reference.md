---
type: scenario
name: cross-reference
feature: introspection
version: 0.1.0
---

# Cross-Reference Scenarios

Scenarios for navigating code relationships using who-calls and who-references.

## Who-Calls Scenarios

### Scenario: Find Callers of a Common Function

**Given** MAPCAR is widely used in loaded systems
**When** I call who-calls with name="mapcar" and package="CL"
**Then** the response lists functions that call MAPCAR
  - Results are fully qualified (PACKAGE::NAME)
  - Results are sorted alphabetically

### Scenario: Find Callers of a Project Function

**Given** CL-MCP-SERVER is loaded
**When** I call who-calls with name="call-tool" and package="CL-MCP-SERVER.TOOLS"
**Then** the response lists internal callers
  - Shows which parts of the server invoke tools

### Scenario: Function with No Callers

**Given** I define an unused function `my-lonely-fn`
**When** I call who-calls with name="my-lonely-fn"
**Then** the response indicates "No callers found for CL-USER::MY-LONELY-FN"

### Scenario: Symbol Not Found for Who-Calls

**When** I call who-calls with name="nonexistent-fn"
**Then** the response indicates the symbol was not found

### Scenario: Package Not Found for Who-Calls

**When** I call who-calls with name="test" and package="NONEXISTENT-PKG"
**Then** the response indicates the package was not found

## Who-References Scenarios

### Scenario: Find References to a Standard Variable

**Given** *PRINT-BASE* is used in formatting code
**When** I call who-references with name="*print-base*" and package="CL"
**Then** the response lists functions that reference this variable

### Scenario: Find References to a Project Variable

**Given** CL-MCP-SERVER has special variables
**When** I call who-references with name="*session*" and package="CL-MCP-SERVER.SESSION"
**Then** the response lists functions that read *SESSION*

### Scenario: Variable with No References

**Given** I define an unused variable `*my-unused-var*`
**When** I call who-references with name="*my-unused-var*"
**Then** the response indicates "No references found"

### Scenario: Symbol Not Found for Who-References

**When** I call who-references with name="*nonexistent-var*"
**Then** the response indicates the symbol was not found

## Cross-Reference Data Completeness

### Scenario: Newly Compiled Code Has Xref Data

**Given** I compile a new function that calls MAPCAR
**When** I call who-calls with name="mapcar" and package="CL"
**Then** my new function appears in the callers list

### Scenario: Interpreted Code May Lack Xref Data

**Given** I evaluate (not compile) a function that calls MAPCAR
**When** I call who-calls with name="mapcar" and package="CL"
**Then** the function may or may not appear (depends on SBCL optimization level)
