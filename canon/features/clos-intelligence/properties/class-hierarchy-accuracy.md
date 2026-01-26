---
type: property
name: class-hierarchy-accuracy
version: 0.1.0
feature: clos-intelligence
covers:
  - contracts/class-info-tool
relates_to:
  - scenarios/hierarchy-navigation
---

# Class Hierarchy Accuracy Property

## Statement

**For all** CLOS classes in the image,
**if** `class-info` is called on that class,
**then** the reported superclasses, subclasses, and class precedence list (CPL) MUST accurately reflect the current class hierarchy in the Lisp image.

## Formal Expression

```
∀ class ∈ Classes, ∀ package ∈ Packages :
  (class-exists(class, package)) ⟹
    let info = class-info(class, package)
    then hierarchy-accurate(info, class)

where:
  hierarchy-accurate(info, class) ≡
    (info.direct-superclasses = sb-mop:class-direct-superclasses(class)) ∧
    (info.direct-subclasses = sb-mop:class-direct-subclasses(class)) ∧
    (info.cpl = sb-mop:class-precedence-list(class)) ∧
    (cpl-linearization-valid(info.cpl))

  cpl-linearization-valid(cpl) ≡
    (∀ c ∈ cpl : supers(c) appear after c in cpl) ∧
    (cpl ends with STANDARD-OBJECT and T for standard classes) ∧
    (no duplicates in cpl)
```

## Informal Explanation

When `class-info` reports class hierarchy information, it must match the CLOS Metaobject Protocol's view of the hierarchy:

1. **Direct Superclasses Accuracy**: The immediate parent classes must be exactly those defined in `defclass`
2. **Direct Subclasses Accuracy**: All loaded subclasses in the image must be reported
3. **CPL Accuracy**: The class precedence list must match the C3 linearization computed by CLOS
4. **CPL Ordering**: Superclasses must appear after their subclasses in the CPL
5. **CPL Completeness**: The CPL must include all ancestors up to T

This property ensures Claude understands the complete inheritance structure for method resolution and slot inheritance.

## Rationale

Accurate hierarchy information is critical for:
- Understanding method resolution order
- Knowing which slots are inherited from which classes
- Navigating the inheritance graph for exploration
- Making correct assumptions about available methods and slots
- Reasoning about multiple inheritance diamond problems

Incorrect hierarchy information leads to faulty reasoning about object-oriented structure.

## Counterexample Shape

If this property is violated, you might see:

**Missing Superclass**:
```lisp
;; Actual definition
(defclass employee (person)
  ((salary :initarg :salary)))

;; Wrong report
class-info("employee") →
  Direct Superclasses:
    - STANDARD-OBJECT  ; VIOLATION! Missing PERSON
```

**Incorrect Subclasses**:
```lisp
;; Actual state
(defclass person () ())
(defclass employee (person) ())
(defclass customer (person) ())

;; Wrong report
class-info("person") →
  Direct Subclasses:
    - EMPLOYEE  ; VIOLATION! Missing CUSTOMER
```

**Wrong CPL Order**:
```lisp
;; Actual definition
(defclass flying-car (car aircraft) ())

;; Correct CPL
(sb-mop:class-precedence-list flying-car)
→ (FLYING-CAR CAR AIRCRAFT VEHICLE STANDARD-OBJECT T)

;; Wrong report
class-info("flying-car") →
  Class Precedence List:
    FLYING-CAR → AIRCRAFT → CAR → VEHICLE → STANDARD-OBJECT → T
    ; VIOLATION! Wrong linearization order
```

**Missing Ancestors in CPL**:
```lisp
;; Actual hierarchy
(defclass vehicle () ())
(defclass car (vehicle) ())
(defclass sedan (car) ())

;; Wrong report
class-info("sedan") →
  Class Precedence List:
    SEDAN → CAR → STANDARD-OBJECT → T
    ; VIOLATION! Missing VEHICLE
```

**Stale Subclass List**:
```lisp
;; Initial state
(defclass person () ())
(defclass employee (person) ())

class-info("person") →
  Direct Subclasses: EMPLOYEE  ; Correct

;; State changes
(defclass customer (person) ())

;; Wrong report (stale)
class-info("person") →
  Direct Subclasses: EMPLOYEE  ; VIOLATION! Missing CUSTOMER
```

## Verification Approach

**Generator**: Create class hierarchies with known inheritance patterns

**Assertion**:
```lisp
(defun verify-class-hierarchy-accuracy (class-name)
  ;; Get actual MOP data
  (let* ((class (find-class class-name))
         (actual-supers (sb-mop:class-direct-superclasses class))
         (actual-subs (sb-mop:class-direct-subclasses class))
         (actual-cpl (sb-mop:class-precedence-list class)))

    ;; Query with class-info
    (let ((info (class-info class-name)))

      ;; Verify hierarchy accuracy
      (and (sets-equal (info-direct-superclasses info) actual-supers)
           (sets-equal (info-direct-subclasses info) actual-subs)
           (lists-equal (info-cpl info) actual-cpl)
           (cpl-linearization-valid (info-cpl info))))))
```

**Property Test Strategy**:

1. **Single Inheritance Test**:
   ```lisp
   (defclass vehicle () ())
   (defclass car (vehicle) ())
   (defclass sedan (car) ())

   ;; Verify each class
   (assert (hierarchy-accurate (class-info 'vehicle)))
   (assert (hierarchy-accurate (class-info 'car)))
   (assert (hierarchy-accurate (class-info 'sedan)))

   ;; Verify CPL ordering
   (let ((cpl (get-cpl 'sedan)))
     (assert (member 'sedan cpl))
     (assert (member 'car cpl))
     (assert (member 'vehicle cpl))
     (assert (< (position 'sedan cpl) (position 'car cpl)))
     (assert (< (position 'car cpl) (position 'vehicle cpl))))
   ```

2. **Multiple Inheritance Test**:
   ```lisp
   (defclass car () ())
   (defclass aircraft () ())
   (defclass flying-car (car aircraft) ())

   ;; Verify C3 linearization
   (let ((cpl (get-cpl 'flying-car)))
     (assert (equal (take 4 cpl)
                    '(FLYING-CAR CAR AIRCRAFT)))
     ;; CAR before AIRCRAFT because listed first
   ```

3. **Diamond Inheritance Test**:
   ```lisp
   (defclass vehicle () ())
   (defclass car (vehicle) ())
   (defclass aircraft (vehicle) ())
   (defclass flying-car (car aircraft) ())

   ;; Verify VEHICLE appears only once in CPL
   (let ((cpl (get-cpl 'flying-car)))
     (assert (= (count 'vehicle cpl) 1)))
   ```

4. **Subclass Discovery Test**:
   ```lisp
   (defclass person () ())

   ;; Initially no subclasses
   (assert (null (get-subclasses 'person)))

   ;; Add first subclass
   (defclass employee (person) ())
   (assert (member 'employee (get-subclasses 'person)))

   ;; Add second subclass
   (defclass customer (person) ())
   (assert (= (length (get-subclasses 'person)) 2))
   (assert (member 'customer (get-subclasses 'person)))
   ```

5. **Built-in Class Hierarchy Test**:
   ```lisp
   ;; Verify built-in class hierarchies
   (let ((cpl (get-cpl 'integer)))
     (assert (member 'rational cpl))
     (assert (member 'real cpl))
     (assert (member 'number cpl))
     (assert (eq (last cpl) 't)))
   ```

6. **Cross-Reference with MOP Test**:
   ```lisp
   ;; Every class in the image
   (defun test-all-classes-hierarchy ()
     (dolist (class-name (all-class-names))
       (let* ((class (find-class class-name))
              (info (class-info class-name))
              (mop-supers (sb-mop:class-direct-superclasses class))
              (mop-cpl (sb-mop:class-precedence-list class)))
         ;; Verify against MOP
         (assert (sets-equal (info-supers info) mop-supers))
         (assert (lists-equal (info-cpl info) mop-cpl)))))
   ```

**Edge Cases**:
- Classes with no direct superclasses (should have STANDARD-OBJECT)
- Built-in classes with BUILT-IN-CLASS metaclass
- Classes with circular dependencies (should not happen in valid CLOS)
- Classes defined but not yet finalized
- Classes with only STANDARD-OBJECT as superclass
- Classes loaded from different packages but with same name

**Dynamic Modification Test**:
```lisp
;; Verify info updates when hierarchy changes
(defclass base () ())

(let ((info1 (class-info 'base)))
  (assert (null (info-subclasses info1)))

  ;; Add subclass
  (defclass derived (base) ())

  (let ((info2 (class-info 'base)))
    (assert (member 'derived (info-subclasses info2)))))
```

**Shrinking**: Find minimal class hierarchy that produces incorrect CPL or subclass list
