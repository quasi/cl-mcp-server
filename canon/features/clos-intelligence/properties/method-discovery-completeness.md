---
type: property
name: method-discovery-completeness
version: 0.1.0
feature: clos-intelligence
covers:
  - contracts/find-methods-tool
relates_to:
  - scenarios/method-discovery
---

# Method Discovery Completeness Property

## Statement

**For all** CLOS classes in the image,
**if** `find-methods` is called on that class,
**then** ALL methods specialized on that class MUST be reported with complete information including generic function name, qualifiers, and full specializer list.

## Formal Expression

```
∀ class ∈ Classes, ∀ package ∈ Packages :
  (class-exists(class, package)) ⟹
    let methods = find-methods(class, package)
    let actual-methods = sb-mop:specializer-direct-methods(class)
    then method-discovery-complete(methods, actual-methods)

where:
  method-discovery-complete(reported, actual) ≡
    (count(reported) = count(actual)) ∧
    (∀ method ∈ actual :
      ∃ reported-method ∈ reported :
        (reported-method.generic-function = method-generic-function(method)) ∧
        (reported-method.qualifiers = method-qualifiers(method)) ∧
        (reported-method.specializers = method-specializers(method)) ∧
        (position-in-specializers-accurate(reported-method, class)))

  position-in-specializers-accurate(method, class) ≡
    ∃ i : method.specializers[i] = class
```

## Informal Explanation

When `find-methods` discovers methods for a class, it must:

1. **Method Completeness**: Report ALL methods specialized on that class
2. **Generic Function Identification**: Show which generic function each method belongs to
3. **Qualifier Reporting**: Include method qualifiers (`:before`, `:after`, `:around`)
4. **Specializer List**: Show the complete specializer list for each method
5. **Position Accuracy**: Indicate which parameter position is specialized on the class
6. **Include Inherited Option**: When requested, include methods from superclasses

This property ensures Claude can discover all operations available on a class.

## Rationale

Complete method discovery is critical for:
- Understanding what operations are available on instances
- Exploring the API of a class
- Finding specialized behavior for a type
- Understanding method combination and ordering
- Discovering inherited methods from superclasses
- Reasoning about method applicability

Incomplete method discovery leads to:
- Missing operations in generated code
- Incorrect assumptions about available methods
- Confusion about specialized behavior
- Failure to discover inherited functionality

## Counterexample Shape

If this property is violated, you might see:

**Missing Method**:
```lisp
;; Actual definitions
(defgeneric process (obj))
(defmethod process ((p person)) ...)
(defmethod process ((e employee)) ...)

;; Wrong report
find-methods("person") →
  Methods specialized on PERSON (0):
    (none)
  ; VIOLATION! Missing PROCESS method
```

**Missing Qualifier**:
```lisp
;; Actual definitions
(defmethod initialize-instance :after ((p person) &key) ...)

;; Wrong report
find-methods("person") →
  INITIALIZE-INSTANCE (PERSON)
  ; VIOLATION! Missing :AFTER qualifier
```

**Incomplete Specializers**:
```lisp
;; Actual definition
(defmethod compare ((p1 person) (p2 person)) ...)

;; Wrong report
find-methods("person") →
  COMPARE (PERSON)
  ; VIOLATION! Second PERSON specializer not shown
```

**Missing Multi-Method Specialization**:
```lisp
;; Actual definition
(defmethod combine ((a integer) (b person)) ...)
(defmethod combine ((a person) (b integer)) ...)

;; Wrong report
find-methods("person") →
  COMBINE (PERSON INTEGER)
  ; VIOLATION! Missing second COMBINE method where PERSON is 2nd param
```

**Missing Inherited Methods**:
```lisp
;; Actual definitions
(defclass person () ())
(defclass employee (person) ())
(defmethod salary ((p person)) ...)

;; Wrong report (with include-inherited: true)
find-methods("employee", include-inherited: true) →
  Methods specialized on EMPLOYEE (0):
    (none)
  ; VIOLATION! Missing inherited SALARY method from PERSON
```

**Missing Standard Methods**:
```lisp
;; Actual definition
(defclass person ()
  ((name :initarg :name :accessor person-name)))

;; Wrong report
find-methods("person") →
  Methods specialized on PERSON (0):
    (none)
  ; VIOLATION! Missing PERSON-NAME and (SETF PERSON-NAME) methods
```

## Verification Approach

**Generator**: Create classes with various method specializations

**Assertion**:
```lisp
(defun verify-method-discovery-completeness (class-name &key include-inherited)
  ;; Get actual MOP methods
  (let* ((class (find-class class-name))
         (actual-methods (if include-inherited
                             (collect-all-methods-on-class class)
                             (sb-mop:specializer-direct-methods class))))

    ;; Query with find-methods
    (let ((reported (find-methods class-name
                                  :include-inherited include-inherited)))

      ;; Verify method completeness
      (and (= (length reported) (length actual-methods))
           (every (lambda (actual-method)
                    (method-reported-p actual-method reported))
                  actual-methods)))))

(defun method-reported-p (mop-method reported-methods)
  "Check if MOP method is present in reported methods."
  (find-if (lambda (reported)
             (and (equal (method-gf-name reported)
                        (generic-function-name
                         (method-generic-function mop-method)))
                  (equal (method-qualifiers reported)
                        (method-qualifiers mop-method))
                  (equal (method-specializers reported)
                        (method-specializers mop-method))))
           reported-methods))
```

**Property Test Strategy**:

1. **Basic Method Discovery Test**:
   ```lisp
   (defclass person () ())
   (defmethod greet ((p person))
     "Hello")

   (let ((methods (find-methods 'person)))
     (assert (= (length methods) 1))
     (assert (member 'greet (mapcar #'method-gf-name methods))))
   ```

2. **Multiple Methods Test**:
   ```lisp
   (defclass person () ())
   (defmethod process ((p person)) ...)
   (defmethod save ((p person)) ...)
   (defmethod validate ((p person)) ...)

   (let ((methods (find-methods 'person)))
     (assert (= (length methods) 3))
     (assert (member 'process (mapcar #'method-gf-name methods)))
     (assert (member 'save (mapcar #'method-gf-name methods)))
     (assert (member 'validate (mapcar #'method-gf-name methods))))
   ```

3. **Method Qualifiers Test**:
   ```lisp
   (defclass person () ())
   (defmethod initialize-instance :before ((p person) &key) ...)
   (defmethod initialize-instance :after ((p person) &key) ...)
   (defmethod initialize-instance :around ((p person) &key) ...)

   (let ((methods (find-methods 'person)))
     (let ((init-methods (remove-if-not
                          (lambda (m)
                            (eq (method-gf-name m)
                                'initialize-instance))
                          methods)))
       (assert (= (length init-methods) 3))
       (assert (member :before (mapcar #'method-qualifiers init-methods)))
       (assert (member :after (mapcar #'method-qualifiers init-methods)))
       (assert (member :around (mapcar #'method-qualifiers init-methods)))))
   ```

4. **Multi-Parameter Specialization Test**:
   ```lisp
   (defclass person () ())
   (defmethod compare ((p1 person) (p2 person)) ...)
   (defmethod transfer ((from person) (to person)) ...)

   (let ((methods (find-methods 'person)))
     ;; Each method should appear once, even though specialized on 2 params
     (assert (member 'compare (mapcar #'method-gf-name methods)))
     (assert (member 'transfer (mapcar #'method-gf-name methods)))

     ;; Verify specializers show both positions
     (let ((compare-method (find-method-by-gf 'compare methods)))
       (assert (equal (method-specializers compare-method)
                      '(person person)))))
   ```

5. **Accessor Method Discovery Test**:
   ```lisp
   (defclass person ()
     ((name :accessor person-name)))

   (let ((methods (find-methods 'person)))
     ;; Should find reader and writer methods
     (assert (member 'person-name (mapcar #'method-gf-name methods)))
     (assert (member '(setf person-name) (mapcar #'method-gf-name methods))))
   ```

6. **Inherited Methods Test**:
   ```lisp
   (defclass person () ())
   (defmethod greet ((p person)) ...)

   (defclass employee (person) ())
   (defmethod work ((e employee)) ...)

   ;; Without inheritance
   (let ((methods (find-methods 'employee :include-inherited nil)))
     (assert (= (length methods) 1))
     (assert (eq (method-gf-name (first methods)) 'work)))

   ;; With inheritance
   (let ((methods (find-methods 'employee :include-inherited t)))
     (assert (>= (length methods) 2))
     (assert (member 'greet (mapcar #'method-gf-name methods)))
     (assert (member 'work (mapcar #'method-gf-name methods))))
   ```

7. **Standard Generic Function Test**:
   ```lisp
   (defclass person () ())

   (let ((methods (find-methods 'person)))
     ;; Should find standard methods like print-object
     (assert (member 'print-object (mapcar #'method-gf-name methods))))
   ```

8. **Method Combination Test**:
   ```lisp
   (defclass person () ())
   (defmethod process ((p person)) ...)
   (defmethod process :before ((p person)) ...)
   (defmethod process :after ((p person)) ...)

   (let ((methods (find-methods 'person)))
     (let ((process-methods (remove-if-not
                             (lambda (m)
                               (eq (method-gf-name m) 'process))
                             methods)))
       ;; Should find primary + before + after = 3 methods
       (assert (= (length process-methods) 3))))
   ```

9. **Cross-Reference with MOP Test**:
   ```lisp
   ;; For every class in the image
   (defun test-all-methods-found ()
     (dolist (class-name (all-class-names))
       (let* ((class (find-class class-name))
              (mop-methods (sb-mop:specializer-direct-methods class))
              (reported (find-methods class-name)))

         ;; Verify count matches
         (assert (= (length reported) (length mop-methods)))

         ;; Verify each MOP method is reported
         (dolist (mop-method mop-methods)
           (assert (method-reported-p mop-method reported))))))
   ```

10. **Method Position in Specializers Test**:
    ```lisp
    (defclass person () ())
    (defclass product () ())
    (defmethod combine ((p person) (pr product)) ...)
    (defmethod combine ((pr product) (p person)) ...)

    (let ((person-methods (find-methods 'person)))
      ;; Should find both COMBINE methods
      (assert (= (count 'combine
                       (mapcar #'method-gf-name person-methods))
                 2)))
    ```

**Edge Cases**:
- Classes with no methods defined
- Generic functions with multiple methods on same class but different qualifiers
- Methods specialized on built-in classes (INTEGER, STRING, etc.)
- Methods with EQL specializers
- Methods defined on metaclasses
- Anonymous generic functions
- Methods removed after being defined
- Methods with method combinations other than standard

**Dynamic Modification Test**:
```lisp
;; Verify discovery updates when methods change
(defclass person () ())

(let ((methods1 (find-methods 'person)))
  (assert (= (length methods1) 0))  ; Initially no methods

  ;; Add method
  (defmethod greet ((p person)) "Hello")

  (let ((methods2 (find-methods 'person)))
    (assert (member 'greet (mapcar #'method-gf-name methods2))))

  ;; Remove method
  (remove-method #'greet
                 (find-method #'greet nil (list (find-class 'person))))

  (let ((methods3 (find-methods 'person)))
    (assert (not (member 'greet (mapcar #'method-gf-name methods3))))))
```

**Shrinking**: Find minimal method definition that is not discovered by find-methods
