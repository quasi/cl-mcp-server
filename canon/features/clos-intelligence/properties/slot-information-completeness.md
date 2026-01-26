---
type: property
name: slot-information-completeness
version: 0.1.0
feature: clos-intelligence
covers:
  - contracts/class-info-tool
relates_to:
  - scenarios/slot-inspection
---

# Slot Information Completeness Property

## Statement

**For all** CLOS classes with defined slots,
**if** `class-info` is called on that class,
**then** ALL direct slots MUST be reported with complete information including name, type, allocation, initargs, initform, and accessors.

## Formal Expression

```
∀ class ∈ Classes, ∀ package ∈ Packages :
  (class-exists(class, package)) ⟹
    let info = class-info(class, package)
    let actual-slots = sb-mop:class-direct-slots(class)
    then slot-info-complete(info, actual-slots)

where:
  slot-info-complete(info, actual-slots) ≡
    (count(info.direct-slots) = count(actual-slots)) ∧
    (∀ slot ∈ actual-slots :
      ∃ reported-slot ∈ info.direct-slots :
        (reported-slot.name = sb-mop:slot-definition-name(slot)) ∧
        (reported-slot.type = sb-mop:slot-definition-type(slot)) ∧
        (reported-slot.allocation = sb-mop:slot-definition-allocation(slot)) ∧
        (reported-slot.initargs = sb-mop:slot-definition-initargs(slot)) ∧
        (reported-slot.initform = sb-mop:slot-definition-initform(slot)) ∧
        (reported-slot.readers = sb-mop:slot-definition-readers(slot)) ∧
        (reported-slot.writers = sb-mop:slot-definition-writers(slot)))

  accessor-functions-exist(accessors) ≡
    ∀ accessor ∈ accessors : fboundp(accessor)
```

## Informal Explanation

When `class-info` reports slot information, it must include all direct slots with complete details:

1. **Slot Name Completeness**: Every direct slot must be reported
2. **Type Information**: Type constraints must be shown (if specified)
3. **Allocation**: `:instance` or `:class` allocation must be accurate
4. **Initargs**: All initialization keywords must be listed
5. **Initform**: Default value forms must be reported (if present)
6. **Readers**: All reader functions must be identified
7. **Writers**: All writer functions (including accessors) must be identified
8. **Accessor Detection**: Combined reader/writer functions must be recognized as accessors

This property ensures Claude has complete information for understanding class structure and object initialization.

## Rationale

Complete slot information is essential for:
- Understanding how to create instances with `make-instance`
- Knowing which keyword arguments are valid
- Identifying how to read and modify slot values
- Understanding slot types for type-aware code generation
- Knowing default values for optional parameters
- Distinguishing instance vs. class slots

Incomplete slot information leads to:
- Incorrect `make-instance` calls
- Missing required initargs
- Wrong accessor function names
- Confusion about slot types and constraints

## Counterexample Shape

If this property is violated, you might see:

**Missing Slot**:
```lisp
;; Actual definition
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age)))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    NAME
      Initarg: :NAME
      Accessor: PERSON-NAME
  ; VIOLATION! Missing AGE slot
```

**Missing Type Information**:
```lisp
;; Actual definition
(defclass person ()
  ((age :type (integer 0 120)
        :initarg :age)))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    AGE
      Initarg: :AGE
  ; VIOLATION! Missing type information
```

**Missing Initform**:
```lisp
;; Actual definition
(defclass person ()
  ((status :initform :active
           :initarg :status)))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    STATUS
      Initarg: :STATUS
  ; VIOLATION! Missing initform :ACTIVE
```

**Wrong Accessor Classification**:
```lisp
;; Actual definition
(defclass person ()
  ((name :initarg :name
         :reader person-name
         :writer set-person-name)))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    NAME
      Accessor: PERSON-NAME  ; VIOLATION! It's a reader, not accessor
```

**Missing Multiple Initargs**:
```lisp
;; Actual definition
(defclass person ()
  ((name :initarg :name
         :initarg :full-name
         :accessor person-name)))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    NAME
      Initarg: :NAME
      Accessor: PERSON-NAME
  ; VIOLATION! Missing :FULL-NAME initarg
```

**Class Slot Not Identified**:
```lisp
;; Actual definition
(defclass person ()
  ((species :allocation :class
            :initform "Homo sapiens")))

;; Wrong report
class-info("person") →
  Direct Slots (1):
    SPECIES
      Initform: "Homo sapiens"
  ; VIOLATION! Missing allocation :CLASS
```

## Verification Approach

**Generator**: Create classes with various slot configurations

**Assertion**:
```lisp
(defun verify-slot-information-completeness (class-name)
  ;; Get actual MOP slot data
  (let* ((class (find-class class-name))
         (actual-slots (sb-mop:class-direct-slots class)))

    ;; Query with class-info
    (let ((info (class-info class-name)))

      ;; Verify slot completeness
      (and (= (length (info-slots info))
              (length actual-slots))
           (every (lambda (slot)
                    (slot-fully-reported-p slot info))
                  actual-slots)))))

(defun slot-fully-reported-p (mop-slot info)
  "Check if MOP slot is fully reported in class-info output."
  (let* ((slot-name (sb-mop:slot-definition-name mop-slot))
         (reported (find-slot-in-info slot-name info)))
    (and reported
         (equal (slot-type reported)
                (sb-mop:slot-definition-type mop-slot))
         (equal (slot-initargs reported)
                (sb-mop:slot-definition-initargs mop-slot))
         (equal (slot-allocation reported)
                (sb-mop:slot-definition-allocation mop-slot))
         (equal (slot-readers reported)
                (sb-mop:slot-definition-readers mop-slot))
         (equal (slot-writers reported)
                (sb-mop:slot-definition-writers mop-slot)))))
```

**Property Test Strategy**:

1. **Basic Slot Test**:
   ```lisp
   (defclass simple ()
     ((field :initarg :field)))

   (let ((info (class-info 'simple)))
     (assert (= (length (info-slots info)) 1))
     (let ((slot (first (info-slots info))))
       (assert (eq (slot-name slot) 'field))
       (assert (member :field (slot-initargs slot)))))
   ```

2. **Complete Slot Features Test**:
   ```lisp
   (defclass complete ()
     ((field :type string
             :allocation :instance
             :initarg :field
             :initform ""
             :accessor complete-field
             :documentation "A field")))

   (let* ((info (class-info 'complete))
          (slot (first (info-slots info))))
     (assert (eq (slot-name slot) 'field))
     (assert (equal (slot-type slot) 'string))
     (assert (eq (slot-allocation slot) :instance))
     (assert (member :field (slot-initargs slot)))
     (assert (equal (slot-initform slot) ""))
     (assert (member 'complete-field (slot-accessors slot))))
   ```

3. **Multiple Initargs Test**:
   ```lisp
   (defclass multi-initarg ()
     ((name :initarg :name
            :initarg :full-name
            :initarg :display-name)))

   (let* ((info (class-info 'multi-initarg))
          (slot (first (info-slots info))))
     (assert (= (length (slot-initargs slot)) 3))
     (assert (member :name (slot-initargs slot)))
     (assert (member :full-name (slot-initargs slot)))
     (assert (member :display-name (slot-initargs slot))))
   ```

4. **Reader/Writer Distinction Test**:
   ```lisp
   (defclass rw-test ()
     ((r-only :reader get-r-only)
      (w-only :writer set-w-only)
      (both :accessor get-both)))

   (let ((info (class-info 'rw-test)))
     (let ((r-slot (find-slot 'r-only info)))
       (assert (member 'get-r-only (slot-readers r-slot)))
       (assert (null (slot-writers r-slot))))

     (let ((w-slot (find-slot 'w-only info)))
       (assert (null (slot-readers w-slot)))
       (assert (member '(setf set-w-only) (slot-writers w-slot))))

     (let ((both-slot (find-slot 'both info)))
       (assert (member 'get-both (slot-readers both-slot)))
       (assert (member '(setf get-both) (slot-writers both-slot)))))
   ```

5. **Class Allocation Test**:
   ```lisp
   (defclass with-class-slot ()
     ((instance-slot :allocation :instance
                     :initarg :inst)
      (class-slot :allocation :class
                  :initform 42)))

   (let ((info (class-info 'with-class-slot)))
     (let ((inst-slot (find-slot 'instance-slot info)))
       (assert (eq (slot-allocation inst-slot) :instance)))

     (let ((cls-slot (find-slot 'class-slot info)))
       (assert (eq (slot-allocation cls-slot) :class))
       (assert (equal (slot-initform cls-slot) 42))))
   ```

6. **Type Constraints Test**:
   ```lisp
   (defclass typed ()
     ((simple :type integer)
      (compound :type (or string null))
      (complex :type (integer 0 100))))

   (let ((info (class-info 'typed)))
     (assert (equal (slot-type (find-slot 'simple info))
                    'integer))
     (assert (equal (slot-type (find-slot 'compound info))
                    '(or string null)))
     (assert (equal (slot-type (find-slot 'complex info))
                    '(integer 0 100))))
   ```

7. **Cross-Reference with MOP Test**:
   ```lisp
   ;; For every class with slots
   (defun test-all-slots-complete ()
     (dolist (class-name (classes-with-slots))
       (let* ((class (find-class class-name))
              (info (class-info class-name))
              (mop-slots (sb-mop:class-direct-slots class)))

         ;; Verify count
         (assert (= (length (info-slots info))
                    (length mop-slots)))

         ;; Verify each slot
         (dolist (mop-slot mop-slots)
           (assert (slot-fully-reported-p mop-slot info))))))
   ```

8. **Inherited vs. Direct Slots Test**:
   ```lisp
   (defclass base ()
     ((base-slot :initarg :base)))

   (defclass derived (base)
     ((derived-slot :initarg :derived)))

   ;; Direct slots should not include inherited
   (let ((info (class-info 'derived)))
     (assert (= (length (info-direct-slots info)) 1))
     (assert (eq (slot-name (first (info-direct-slots info)))
                 'derived-slot))

     ;; But all-slots count should include both
     (assert (>= (info-all-slots-count info) 2)))
   ```

**Edge Cases**:
- Slots with no initargs (uninitialized slots)
- Slots with complex type expressions
- Slots with unbound initforms
- Slots with lambda expressions as initforms
- Slots with multiple readers/writers
- Inherited slots (should not appear in direct-slots)
- Built-in classes with no inspectable slots
- Slots with documentation strings

**Completeness Test**:
```lisp
;; Verify no information is lost from MOP
(defun test-slot-information-lossless (class-name)
  (let* ((class (find-class class-name))
         (mop-slots (sb-mop:class-direct-slots class))
         (info (class-info class-name)))

    (dolist (mop-slot mop-slots)
      ;; Every MOP attribute must be captured
      (let ((reported (find-slot-by-name
                       (sb-mop:slot-definition-name mop-slot)
                       info)))
        (assert reported "Slot not reported")
        (assert (slot-attributes-match mop-slot reported))))))
```

**Shrinking**: Find minimal slot definition that is incompletely reported
