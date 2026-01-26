---
type: verification
name: class-hierarchy-accuracy-property-test
source: properties/class-hierarchy-accuracy.md
level: property
tags:
  - property-based
  - clos
  - hierarchy
---

# Property Test: Class Hierarchy Accuracy

## Purpose

Verify that class hierarchy relationships (superclasses, subclasses, CPL) are accurately reported.

## Prerequisites

- Initialized MCP server
- CLOS intelligence tools available

## Implementation

### Property: Transitive Superclass Relationships

```lisp
(deftest class-hierarchy-transitivity ()
  "If A inherits B and B inherits C, then C is in A's CPL"
  (dotimes (i 20)
    ;; Define class chain
    (let ((base (format nil "CLASS-BASE-~A" i))
          (middle (format nil "CLASS-MIDDLE-~A" i))
          (derived (format nil "CLASS-DERIVED-~A" i)))

      ;; Create classes
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A () ())" base))))
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A (~A) ())" middle base))))
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A (~A) ())" derived middle))))

      ;; Check derived class
      (let* ((response (call-tool *test-server* "class-info"
                                 `(("class" . ,derived))))
             (content (result-content response)))

        ;; CPL should include all ancestors
        (is (search base content :test #'char-equal)
            "Base class ~A not in ~A's CPL" base derived)
        (is (search middle content :test #'char-equal)
            "Middle class ~A not in ~A's CPL" middle derived)))))
```

### Property: Subclass Relationships

```lisp
(deftest class-hierarchy-subclasses ()
  "Subclass reported in parent's subclass list"
  (dotimes (i 10)
    (let ((parent (format nil "PARENT-~A" i))
          (child (format nil "CHILD-~A" i)))

      ;; Create classes
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A () ())" parent))))
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A (~A) ())" child parent))))

      ;; Check parent
      (let* ((response (call-tool *test-server* "class-info"
                                 `(("class" . ,parent))))
             (content (result-content response)))

        ;; Should list child as subclass
        (is (search child content :test #'char-equal)
            "Child class ~A not listed in ~A's subclasses" child parent)))))
```

### Property: CPL Includes STANDARD-OBJECT

```lisp
(deftest class-hierarchy-standard-object ()
  "All user classes have STANDARD-OBJECT in CPL"
  (dotimes (i 20)
    (let ((classname (format nil "USER-CLASS-~A" i)))

      ;; Create class
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A () ())" classname))))

      ;; Check CPL
      (let* ((response (call-tool *test-server* "class-info"
                                 `(("class" . ,classname))))
             (content (result-content response)))

        ;; Must include STANDARD-OBJECT
        (is (search "STANDARD-OBJECT" content :test #'char-equal)
            "Class ~A CPL missing STANDARD-OBJECT" classname)))))
```

### Property: Multiple Inheritance Linearization

```lisp
(deftest class-hierarchy-multiple-inheritance ()
  "Multiple inheritance produces valid CPL"
  (dotimes (i 5)
    (let ((mixin-a (format nil "MIXIN-A-~A" i))
          (mixin-b (format nil "MIXIN-B-~A" i))
          (combined (format nil "COMBINED-~A" i)))

      ;; Create mixins
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A () ())" mixin-a))))
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A () ())" mixin-b))))

      ;; Create combined class
      (call-tool *test-server* "evaluate-lisp"
                `(("code" . ,(format nil "(defclass ~A (~A ~A) ())"
                                   combined mixin-a mixin-b))))

      ;; Check CPL
      (let* ((response (call-tool *test-server* "class-info"
                                 `(("class" . ,combined))))
             (content (result-content response)))

        ;; Both mixins should appear
        (is (search mixin-a content :test #'char-equal)
            "Mixin A missing from multiple inheritance CPL")
        (is (search mixin-b content :test #'char-equal)
            "Mixin B missing from multiple inheritance CPL")))))
```

## Configuration

- Examples: 10-20 per test
- Cover simple and complex hierarchies
- Test multiple inheritance cases

## Notes

- CPL (Class Precedence List) must follow CLOS linearization rules
- All user classes inherit from STANDARD-OBJECT
- Subclass relationships bidirectional (parent knows children)
- Multiple inheritance order matters
