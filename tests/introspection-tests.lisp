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
