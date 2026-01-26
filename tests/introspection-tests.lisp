;;; tests/introspection-tests.lisp
;;; ABOUTME: Tests for Phase A introspection tools

(in-package #:cl-mcp-server-tests)

(def-suite introspection-tests
  :description "Tests for symbol introspection tools"
  :in cl-mcp-server-tests)

(in-suite introspection-tests)

;;; ==========================================================================
;;; Symbol Type Classification Tests
;;; ==========================================================================

(test symbol-type-function
  "symbol-type-info identifies regular functions"
  (is (eq :function (cl-mcp-server.introspection:symbol-type-info 'car))))

(test symbol-type-macro
  "symbol-type-info identifies macros"
  (is (eq :macro (cl-mcp-server.introspection:symbol-type-info 'defun))))

(test symbol-type-generic-function
  "symbol-type-info identifies generic functions"
  (is (eq :generic-function
          (cl-mcp-server.introspection:symbol-type-info 'print-object))))

(test symbol-type-class
  "symbol-type-info identifies classes"
  (is (eq :class (cl-mcp-server.introspection:symbol-type-info 'standard-class))))

(test symbol-type-variable
  "symbol-type-info identifies bound variables"
  (is (eq :variable (cl-mcp-server.introspection:symbol-type-info '*package*))))

(test symbol-type-unbound
  "symbol-type-info returns :symbol for unbound symbols"
  (let ((sym (gensym "TEST-")))
    (is (eq :symbol (cl-mcp-server.introspection:symbol-type-info sym)))))

;;; ==========================================================================
;;; describe-symbol Tests
;;; ==========================================================================

(test introspect-symbol-function
  "introspect-symbol returns correct info for functions"
  (let ((info (cl-mcp-server.introspection:introspect-symbol 'mapcar)))
    (is (string= "MAPCAR" (getf info :name)))
    (is (string= "COMMON-LISP" (getf info :package)))
    (is (eq :function (getf info :type)))
    (is (not (null (getf info :arglist))))
    (is (not (null (getf info :documentation))))))

(test introspect-symbol-variable
  "introspect-symbol returns value for variables"
  (let ((info (cl-mcp-server.introspection:introspect-symbol '*print-base*)))
    (is (eq :variable (getf info :type)))
    (is (not (null (getf info :value))))))

(test introspect-symbol-macro
  "introspect-symbol returns arglist for macros"
  (let ((info (cl-mcp-server.introspection:introspect-symbol 'defun)))
    (is (eq :macro (getf info :type)))
    (is (not (null (getf info :arglist))))))

(test format-symbol-info-includes-name
  "format-symbol-info includes symbol name"
  (let* ((info (cl-mcp-server.introspection:introspect-symbol 'car))
         (formatted (cl-mcp-server.introspection:format-symbol-info info)))
    (is (search "CAR" formatted))))

;;; ==========================================================================
;;; apropos-search Tests
;;; ==========================================================================

(test introspect-apropos-finds-matches
  "introspect-apropos finds matching symbols"
  (let ((results (cl-mcp-server.introspection:introspect-apropos "mapcar")))
    (is (not (null results)))
    (is (find "MAPCAR" results :key (lambda (r) (getf r :name)) :test #'string=))))

(test introspect-apropos-case-insensitive
  "introspect-apropos is case-insensitive"
  (let ((results (cl-mcp-server.introspection:introspect-apropos "MAPCAR")))
    (is (not (null results)))))

(test introspect-apropos-filters-by-type
  "introspect-apropos filters by type"
  (let ((results (cl-mcp-server.introspection:introspect-apropos "def" :type :macro)))
    (is (every (lambda (r) (eq :macro (getf r :type))) results))))

(test introspect-apropos-filters-by-package
  "introspect-apropos limits to specific package"
  (let ((results (cl-mcp-server.introspection:introspect-apropos
                  "evaluate" :package "CL-MCP-SERVER.EVALUATOR")))
    (is (every (lambda (r)
                 (string= "CL-MCP-SERVER.EVALUATOR" (getf r :package)))
               results))))

(test format-apropos-results-includes-count
  "format-apropos-results includes match count"
  (let* ((results (cl-mcp-server.introspection:introspect-apropos "car"))
         (formatted (cl-mcp-server.introspection:format-apropos-results results "car")))
    (is (search "Found" formatted))
    (is (search "symbol" formatted))))

;;; ==========================================================================
;;; who-calls Tests
;;; ==========================================================================

(test introspect-who-calls-returns-list
  "introspect-who-calls returns a list"
  (let ((results (cl-mcp-server.introspection:introspect-who-calls
                  'cl-mcp-server.evaluator:evaluate-code)))
    (is (listp results))))

(test introspect-who-calls-includes-caller-info
  "introspect-who-calls results include caller and location"
  (let ((results (cl-mcp-server.introspection:introspect-who-calls
                  'cl-mcp-server.evaluator:evaluate-code)))
    (when results
      (let ((first-result (first results)))
        (is (not (null (getf first-result :caller))))))))

(test format-who-calls-results-includes-count
  "format-who-calls-results includes caller count"
  (let* ((results (cl-mcp-server.introspection:introspect-who-calls
                   'cl-mcp-server.evaluator:evaluate-code))
         (formatted (cl-mcp-server.introspection:format-who-calls-results
                     results 'cl-mcp-server.evaluator:evaluate-code)))
    (is (or (search "caller" formatted)
            (search "No callers" formatted)))))

;;; ==========================================================================
;;; who-references Tests
;;; ==========================================================================

(test introspect-who-references-returns-list
  "introspect-who-references returns a list"
  (let ((results (cl-mcp-server.introspection:introspect-who-references
                  'cl-mcp-server.evaluator:*evaluation-timeout*)))
    (is (listp results))))

(test introspect-who-references-finds-users
  "introspect-who-references finds code that uses a variable"
  (let ((results (cl-mcp-server.introspection:introspect-who-references
                  'cl-mcp-server.evaluator:*evaluation-timeout*)))
    (is (not (null results)))))

;;; ==========================================================================
;;; macroexpand-form Tests
;;; ==========================================================================

(test introspect-macroexpand-basic
  "introspect-macroexpand expands a simple macro"
  (let ((result (cl-mcp-server.introspection:introspect-macroexpand
                 "(when t 42)")))
    (is (not (null (getf result :original))))
    (is (not (null (getf result :expanded))))
    (is (getf result :changed-p))))

(test introspect-macroexpand-no-change
  "introspect-macroexpand reports when no expansion occurs"
  (let ((result (cl-mcp-server.introspection:introspect-macroexpand "(+ 1 2)")))
    (is (not (getf result :changed-p)))))

(test introspect-macroexpand-full
  "introspect-macroexpand with :full expands recursively"
  (let ((result (cl-mcp-server.introspection:introspect-macroexpand
                 "(defun foo () 42)" :full t)))
    (is (getf result :changed-p))
    (is (search "PROGN" (getf result :expanded)))))

(test format-macroexpand-result-shows-both
  "format-macroexpand-result shows original and expanded"
  (let* ((result (cl-mcp-server.introspection:introspect-macroexpand "(when t 1)"))
         (formatted (cl-mcp-server.introspection:format-macroexpand-result result)))
    (is (search "Original:" formatted))
    (is (search "Expanded:" formatted))))

;;; ==========================================================================
;;; Tool Registration Tests
;;; ==========================================================================

(test describe-symbol-tool-registered
  "describe-symbol tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "describe-symbol")))))

(test apropos-search-tool-registered
  "apropos-search tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "apropos-search")))))

(test who-calls-tool-registered
  "who-calls tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "who-calls")))))

(test who-references-tool-registered
  "who-references tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "who-references")))))

(test macroexpand-form-tool-registered
  "macroexpand-form tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "macroexpand-form")))))

;;; ==========================================================================
;;; Tool Call Tests
;;; ==========================================================================

(test call-describe-symbol-tool
  "calling describe-symbol tool returns info"
  (let ((result (cl-mcp-server.tools:call-tool
                 "describe-symbol"
                 '(("name" . "car") ("package" . "CL"))
                 nil)))
    (is (stringp result))
    (is (search "CAR" result))
    (is (search "FUNCTION" result))))

(test call-apropos-search-tool
  "calling apropos-search tool returns results"
  (let ((result (cl-mcp-server.tools:call-tool
                 "apropos-search"
                 '(("pattern" . "map"))
                 nil)))
    (is (stringp result))
    (is (search "Found" result))))

(test call-macroexpand-form-tool
  "calling macroexpand-form tool expands code"
  (let ((result (cl-mcp-server.tools:call-tool
                 "macroexpand-form"
                 '(("form" . "(when t 1)"))
                 nil)))
    (is (stringp result))
    (is (search "Original:" result))
    (is (search "Expanded:" result))))

;;; ==========================================================================
;;; Error Handling Tests
;;; ==========================================================================

(test describe-symbol-unknown-package
  "describe-symbol handles unknown package"
  (let ((result (cl-mcp-server.tools:call-tool
                 "describe-symbol"
                 '(("name" . "foo") ("package" . "NONEXISTENT-PACKAGE"))
                 nil)))
    (is (search "not found" result))))

(test describe-symbol-unknown-symbol
  "describe-symbol handles unknown symbol"
  (let ((result (cl-mcp-server.tools:call-tool
                 "describe-symbol"
                 '(("name" . "this-symbol-does-not-exist-xyz"))
                 nil)))
    (is (search "not found" result))))

(test macroexpand-form-invalid-form
  "macroexpand-form handles invalid forms"
  (let ((result (cl-mcp-server.tools:call-tool
                 "macroexpand-form"
                 '(("form" . "(defun"))  ; incomplete form
                 nil)))
    (is (search "Error" result))))

;;; ==========================================================================
;;; validate-syntax Tests
;;; ==========================================================================

(test introspect-validate-syntax-valid
  "introspect-validate-syntax returns valid for correct code"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(defun foo (x) (+ x 1))")))
    (is (getf result :valid))
    (is (= 1 (getf result :forms)))))

(test introspect-validate-syntax-multiple-forms
  "introspect-validate-syntax counts multiple forms"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(defun a () 1) (defun b () 2) (defun c () 3)")))
    (is (getf result :valid))
    (is (= 3 (getf result :forms)))))

(test introspect-validate-syntax-empty
  "introspect-validate-syntax handles empty input"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax "")))
    (is (getf result :valid))
    (is (= 0 (getf result :forms)))))

(test introspect-validate-syntax-missing-close
  "introspect-validate-syntax detects missing close paren"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(defun foo (x) (+ x 1)")))
    (is (not (getf result :valid)))
    (is (getf result :error))
    (is (getf result :unclosed-count))))

(test introspect-validate-syntax-extra-close
  "introspect-validate-syntax detects extra close paren"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(defun foo (x) (+ x 1)))")))
    (is (not (getf result :valid)))
    (is (getf result :error))))

(test introspect-validate-syntax-nested-valid
  "introspect-validate-syntax handles deep nesting"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(a (b (c (d (e (f 1))))))")))
    (is (getf result :valid))))

(test introspect-validate-syntax-string-parens
  "introspect-validate-syntax ignores parens in strings"
  (let ((result (cl-mcp-server.introspection:introspect-validate-syntax
                 "(defun foo () \"has ( and ) in it\")")))
    (is (getf result :valid))))

(test format-validate-result-valid
  "format-validate-result shows checkmark for valid code"
  (let* ((result (cl-mcp-server.introspection:introspect-validate-syntax
                  "(+ 1 2)"))
         (formatted (cl-mcp-server.introspection:format-validate-result result)))
    (is (search "valid" formatted :test #'char-equal))))

(test format-validate-result-invalid
  "format-validate-result shows error for invalid code"
  (let* ((result (cl-mcp-server.introspection:introspect-validate-syntax
                  "(+ 1 2"))
         (formatted (cl-mcp-server.introspection:format-validate-result result)))
    (is (search "invalid" formatted :test #'char-equal))
    (is (search "Error" formatted))))

(test validate-syntax-tool-registered
  "validate-syntax tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "validate-syntax")))))

(test call-validate-syntax-tool-valid
  "calling validate-syntax tool with valid code"
  (let ((result (cl-mcp-server.tools:call-tool
                 "validate-syntax"
                 '(("code" . "(defun foo () 1)"))
                 nil)))
    (is (stringp result))
    (is (search "valid" result :test #'char-equal))))

(test call-validate-syntax-tool-invalid
  "calling validate-syntax tool with invalid code"
  (let ((result (cl-mcp-server.tools:call-tool
                 "validate-syntax"
                 '(("code" . "(defun foo () 1"))
                 nil)))
    (is (stringp result))
    (is (search "invalid" result :test #'char-equal))))

;;; ==========================================================================
;;; Phase D: CLOS Intelligence Tests
;;; ==========================================================================

;; Define a test class for CLOS tests
(defclass test-clos-person ()
  ((name :initarg :name :accessor test-person-name :type string)
   (age :initarg :age :accessor test-person-age :initform 0))
  (:documentation "Test class for CLOS introspection tests"))

(test introspect-slot-direct
  "introspect-slot extracts slot info from direct slot"
  (let* ((class (find-class 'test-clos-person))
         (slots (sb-mop:class-direct-slots class))
         (name-slot (find 'name slots :key #'sb-mop:slot-definition-name))
         (info (cl-mcp-server.introspection:introspect-slot name-slot)))
    (is (eq 'name (getf info :name)))
    (is (eq 'string (getf info :type)))
    (is (member :name (getf info :initargs)))
    (is (member 'test-person-name (getf info :readers)))))

(test introspect-class-basic
  "introspect-class returns basic class info"
  (let ((info (cl-mcp-server.introspection:introspect-class 'test-clos-person)))
    (is (eq 'test-clos-person (getf info :name)))
    (is (string= "CL-MCP-SERVER-TESTS" (getf info :package)))
    (is (eq 'standard-class (getf info :metaclass)))
    (is (string= "Test class for CLOS introspection tests"
                 (getf info :documentation)))))

(test introspect-class-slots
  "introspect-class includes slot information"
  (let ((info (cl-mcp-server.introspection:introspect-class 'test-clos-person)))
    (is (= 2 (length (getf info :direct-slots))))
    (is (find 'name (getf info :direct-slots) :key (lambda (s) (getf s :name))))
    (is (find 'age (getf info :direct-slots) :key (lambda (s) (getf s :name))))))

(test introspect-class-superclasses
  "introspect-class includes superclass info"
  (let ((info (cl-mcp-server.introspection:introspect-class 'test-clos-person)))
    (is (member 'standard-object (getf info :superclasses)))))

(test introspect-class-from-string
  "introspect-class works with string designator"
  (let ((info (cl-mcp-server.introspection:introspect-class "test-clos-person")))
    (is (eq 'test-clos-person (getf info :name)))))

(test format-class-info-includes-name
  "format-class-info includes class name"
  (let* ((info (cl-mcp-server.introspection:introspect-class 'test-clos-person))
         (formatted (cl-mcp-server.introspection:format-class-info info)))
    (is (search "TEST-CLOS-PERSON" formatted))
    (is (search "STANDARD-CLASS" formatted))))

(test format-class-info-includes-slots
  "format-class-info includes slot details"
  (let* ((info (cl-mcp-server.introspection:introspect-class 'test-clos-person))
         (formatted (cl-mcp-server.introspection:format-class-info info)))
    (is (search "NAME" formatted))
    (is (search "AGE" formatted))
    (is (search "Direct Slots" formatted))))

(test introspect-find-methods-basic
  "introspect-find-methods finds methods on a class"
  (let ((results (cl-mcp-server.introspection:introspect-find-methods 'test-clos-person)))
    (is (listp results))
    ;; Should find accessor methods
    (is (find "TEST-PERSON-NAME" results
              :key (lambda (m) (getf m :generic-function))
              :test #'string=))))

(test introspect-find-methods-includes-specializers
  "introspect-find-methods includes specializer info"
  (let ((results (cl-mcp-server.introspection:introspect-find-methods 'test-clos-person)))
    (when results
      (let ((first-method (first results)))
        (is (not (null (getf first-method :specializers))))))))

(test format-find-methods-results-format
  "format-find-methods-results produces readable output"
  (let* ((results (cl-mcp-server.introspection:introspect-find-methods 'test-clos-person))
         (formatted (cl-mcp-server.introspection:format-find-methods-results
                     results 'test-clos-person)))
    (is (stringp formatted))
    (is (search "method" formatted))))

;; Tool Registration Tests for Phase D
(test class-info-tool-registered
  "class-info tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "class-info")))))

(test find-methods-tool-registered
  "find-methods tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "find-methods")))))

;; Tool Call Tests for Phase D
(test call-class-info-tool
  "calling class-info tool returns class information"
  (let ((result (cl-mcp-server.tools:call-tool
                 "class-info"
                 '(("class" . "standard-class") ("package" . "CL"))
                 nil)))
    (is (stringp result))
    (is (search "STANDARD-CLASS" result))))

(test call-class-info-tool-unknown-class
  "class-info handles unknown class"
  (let ((result (cl-mcp-server.tools:call-tool
                 "class-info"
                 '(("class" . "nonexistent-class-xyz"))
                 nil)))
    (is (search "not found" result))))

(test call-find-methods-tool
  "calling find-methods tool returns method list"
  (let ((result (cl-mcp-server.tools:call-tool
                 "find-methods"
                 '(("class" . "test-clos-person")
                   ("package" . "CL-MCP-SERVER-TESTS"))
                 nil)))
    (is (stringp result))
    (is (search "method" result))))
