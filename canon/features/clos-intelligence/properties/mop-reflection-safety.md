---
type: property
name: mop-reflection-safety
version: 0.1.0
feature: clos-intelligence
covers:
  - contracts/class-info-tool
  - contracts/find-methods-tool
relates_to:
  - scenarios/class-exploration
---

# MOP Reflection Safety Property

## Statement

**For all** CLOS intelligence tool invocations,
**the tools** MUST NOT modify any class definitions, slot definitions, method definitions, or class hierarchy relationships.

## Formal Expression

```
∀ tool ∈ CLOSIntrospectionTools, ∀ state₀ ∈ ImageState :
  let state₁ = execute(tool, state₀)
  then clos-state(state₁) = clos-state(state₀)

where:
  CLOSIntrospectionTools = {
    class-info,
    find-methods
  }

  clos-state(s) = {
    class-definitions(s),
    slot-definitions(s),
    method-definitions(s),
    class-hierarchy(s),
    class-precedence-lists(s)
  }

  read-only-mop-functions = {
    find-class,
    class-name,
    class-direct-superclasses,
    class-direct-subclasses,
    class-precedence-list,
    class-direct-slots,
    class-slots,
    slot-definition-*,
    specializer-direct-methods,
    method-qualifiers,
    method-specializers,
    method-generic-function
  }
```

## Informal Explanation

All CLOS intelligence tools are purely observational. They must:

1. **Not Modify Classes**: No `ensure-class`, `defclass`, or class redefinition
2. **Not Modify Slots**: No slot addition, removal, or modification
3. **Not Modify Methods**: No `defmethod`, `remove-method`, or method changes
4. **Not Finalize Classes**: No premature class finalization side effects
5. **Not Trigger Side Effects**: No code evaluation during introspection
6. **Use Read-Only MOP**: Only use MOP functions that query, never mutate

This property ensures Claude can safely explore the CLOS object system without risk of accidentally modifying class definitions or breaking existing code.

## Rationale

CLOS intelligence tools are meant for discovery and understanding, not modification. Users expect these tools to be safe to run at any time without altering the object system.

Key safety implications:
- MOP introspection uses read-only functions like `class-direct-slots`, not mutating functions
- No class finalization is triggered unless already finalized
- No methods are added or removed during discovery
- No class hierarchy is altered during exploration
- No slot definitions are modified during inspection

Violating this property would break the "look but don't touch" contract and could:
- Break existing code that depends on current class structure
- Cause unexpected method resolution changes
- Trigger class redefinition warnings
- Invalidate cached compilation results

## Counterexample Shape

If this property is violated, you might see:

**Class Definition Modified**:
```lisp
;; Before tool call
(class-slots (find-class 'person)) → (NAME AGE)

;; Tool call
class-info("person")

;; After tool call
(class-slots (find-class 'person)) → (NAME AGE EMAIL)
; VIOLATION! New slot added
```

**Method Added**:
```lisp
;; Before tool call
(length (generic-function-methods #'process)) → 5

;; Tool call
find-methods("person")

;; After tool call
(length (generic-function-methods #'process)) → 6
; VIOLATION! New method appeared
```

**Class Hierarchy Changed**:
```lisp
;; Before tool call
(class-direct-superclasses (find-class 'employee))
→ (PERSON)

;; Tool call
class-info("employee")

;; After tool call
(class-direct-superclasses (find-class 'employee))
→ (PERSON CONTRACTOR)
; VIOLATION! Superclass added
```

**Premature Finalization**:
```lisp
;; Before tool call
(sb-mop:class-finalized-p (find-class 'my-class)) → NIL

;; Tool call
class-info("my-class")

;; After tool call
(sb-mop:class-finalized-p (find-class 'my-class)) → T
; VIOLATION! Class finalized as side effect
```

**Slot Definition Modified**:
```lisp
;; Before tool call
(slot-definition-type (first (class-direct-slots (find-class 'person))))
→ T

;; Tool call
class-info("person")

;; After tool call
(slot-definition-type (first (class-direct-slots (find-class 'person))))
→ STRING
; VIOLATION! Slot type changed
```

## Verification Approach

**Generator**: Generate random CLOS tool calls with valid inputs

**Assertion**:
```lisp
(defun verify-mop-reflection-safety (tool-name params)
  ;; Capture CLOS state before
  (let ((classes-before (capture-all-classes))
        (methods-before (capture-all-methods))
        (hierarchy-before (capture-class-hierarchy)))

    ;; Execute CLOS tool
    (call-tool tool-name params)

    ;; Verify CLOS state unchanged
    (and (classes-equal classes-before (capture-all-classes))
         (methods-equal methods-before (capture-all-methods))
         (hierarchy-equal hierarchy-before (capture-class-hierarchy)))))

(defun capture-all-classes ()
  "Capture current state of all classes."
  (loop for class being the hash-values of
        (sb-impl::package-internal-symbols (find-package :cl-user))
        when (and (symbolp class) (find-class class nil))
        collect (cons class (capture-class-state (find-class class)))))

(defun capture-class-state (class)
  "Capture immutable state of a class."
  (list :name (class-name class)
        :direct-superclasses (class-direct-superclasses class)
        :direct-subclasses (class-direct-subclasses class)
        :cpl (class-precedence-list class)
        :direct-slots (class-direct-slots class)
        :finalized-p (sb-mop:class-finalized-p class)))
```

**Property Test Strategy**:

1. **Class Definition Preservation Test**:
   ```lisp
   (defclass person ()
     ((name :initarg :name)))

   (let ((slots-before (class-direct-slots (find-class 'person))))
     ;; Query class
     (class-info 'person)

     (let ((slots-after (class-direct-slots (find-class 'person))))
       ;; Verify no slots added/removed
       (assert (equal slots-before slots-after))))
   ```

2. **Method Preservation Test**:
   ```lisp
   (defclass person () ())
   (defmethod greet ((p person)) "Hello")

   (let ((methods-before (sb-mop:specializer-direct-methods
                          (find-class 'person))))
     ;; Query methods
     (find-methods 'person)

     (let ((methods-after (sb-mop:specializer-direct-methods
                           (find-class 'person))))
       ;; Verify no methods added/removed
       (assert (equal methods-before methods-after))))
   ```

3. **Class Hierarchy Preservation Test**:
   ```lisp
   (defclass person () ())
   (defclass employee (person) ())

   (let ((supers-before (class-direct-superclasses
                         (find-class 'employee)))
         (subs-before (class-direct-subclasses
                       (find-class 'person))))

     ;; Query both classes
     (class-info 'person)
     (class-info 'employee)

     (let ((supers-after (class-direct-superclasses
                          (find-class 'employee)))
           (subs-after (class-direct-subclasses
                        (find-class 'person))))

       ;; Verify hierarchy unchanged
       (assert (equal supers-before supers-after))
       (assert (equal subs-before subs-after))))
   ```

4. **Finalization Safety Test**:
   ```lisp
   ;; Create forward-referenced class
   (defclass base () ())
   (defclass derived (base forward-ref) ())

   ;; Class is not finalized due to missing forward-ref
   (let ((finalized-before (sb-mop:class-finalized-p
                            (find-class 'derived))))

     ;; Query the class
     (class-info 'derived)

     (let ((finalized-after (sb-mop:class-finalized-p
                             (find-class 'derived))))

       ;; Verify finalization state unchanged
       (assert (equal finalized-before finalized-after))))
   ```

5. **Slot Definition Preservation Test**:
   ```lisp
   (defclass person ()
     ((name :type string :initarg :name)))

   (let* ((class (find-class 'person))
          (slot-before (first (class-direct-slots class)))
          (type-before (slot-definition-type slot-before)))

     ;; Query class
     (class-info 'person)

     (let* ((slot-after (first (class-direct-slots class)))
            (type-after (slot-definition-type slot-after)))

       ;; Verify slot definition unchanged
       (assert (equal type-before type-after))
       (assert (eq slot-before slot-after))))
   ```

6. **CPL Preservation Test**:
   ```lisp
   (defclass vehicle () ())
   (defclass car (vehicle) ())
   (defclass sedan (car) ())

   (let ((cpl-before (class-precedence-list (find-class 'sedan))))

     ;; Query class
     (class-info 'sedan)

     (let ((cpl-after (class-precedence-list (find-class 'sedan))))

       ;; Verify CPL unchanged
       (assert (equal cpl-before cpl-after))))
   ```

7. **Repeated Query Safety Test**:
   ```lisp
   ;; Verify repeated queries don't accumulate changes
   (defclass person () ())

   (let ((state-0 (capture-class-state (find-class 'person))))
     ;; Query multiple times
     (dotimes (i 100)
       (class-info 'person)
       (find-methods 'person))

     (let ((state-100 (capture-class-state (find-class 'person))))
       ;; State should be identical
       (assert (equal state-0 state-100))))
   ```

8. **Concurrent Query Safety Test**:
   ```lisp
   ;; Verify queries don't interfere with each other
   (defclass person () ())

   (let ((state-before (capture-class-state (find-class 'person))))

     ;; Simulate concurrent queries (in single thread)
     (let ((results (list (class-info 'person)
                         (find-methods 'person)
                         (class-info 'person)
                         (find-methods 'person))))

       (let ((state-after (capture-class-state (find-class 'person))))
         ;; Verify state unchanged
         (assert (equal state-before state-after))
         ;; Verify all results were valid
         (assert (every #'identity results)))))
   ```

9. **Built-in Class Safety Test**:
   ```lisp
   ;; Verify introspection doesn't modify built-in classes
   (let ((state-before (capture-class-state (find-class 'integer))))

     ;; Query built-in class
     (class-info 'integer)
     (find-methods 'integer)

     (let ((state-after (capture-class-state (find-class 'integer))))
       ;; Verify built-in class unchanged
       (assert (equal state-before state-after))))
   ```

10. **Cross-Reference with MOP Test**:
    ```lisp
    ;; Verify all MOP queries are read-only
    (defun test-mop-read-only-usage ()
      ;; Monitor which MOP functions are called
      (let ((called-functions '()))
        (trace-mop-calls
         (lambda ()
           (class-info 'person)
           (find-methods 'person)))

        ;; Verify only read-only MOP functions were used
        (assert (every (lambda (fn)
                        (member fn *read-only-mop-functions*))
                      called-functions))))
    ```

**Edge Cases**:
- Classes not yet finalized
- Classes with forward-referenced superclasses
- Built-in classes (INTEGER, STRING, etc.)
- Standard generic functions (INITIALIZE-INSTANCE, etc.)
- Classes with method combinations
- Classes being redefined during query (race condition)
- Classes with computed slots
- Metaclasses

**Implementation Notes**:
```lisp
;; Safe MOP introspection pattern
(find-class 'person nil)              ; Safe: read-only lookup
(class-direct-slots class)            ; Safe: read-only query
(slot-definition-name slot)           ; Safe: read-only accessor

;; UNSAFE patterns to avoid
(ensure-class 'person ...)            ; UNSAFE: modifies/creates class
(add-direct-subclass parent child)    ; UNSAFE: modifies hierarchy
(remove-direct-method gf method)      ; UNSAFE: modifies methods
(finalize-inheritance class)          ; UNSAFE: triggers side effect
```

**Shrinking**: Find minimal tool invocation that causes CLOS state modification
