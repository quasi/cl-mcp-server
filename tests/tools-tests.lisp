;;; tests/tools-tests.lisp
;;; ABOUTME: Tests for MCP tool definitions and dispatch

(in-package #:cl-mcp-server-tests)

(def-suite tools-tests
  :description "Tests for MCP tool definitions"
  :in cl-mcp-server-tests)

(in-suite tools-tests)

;;; ==========================================================================
;;; Tool Definition Tests
;;; ==========================================================================

(test tool-definition-structure
  "Test that tool definitions have required fields"
  (let ((tool (cl-mcp-server.tools:get-tool "evaluate-lisp")))
    (is (not (null tool)))
    (is (stringp (cl-mcp-server.tools:tool-name tool)))
    (is (stringp (cl-mcp-server.tools:tool-description tool)))
    (is (listp (cl-mcp-server.tools:tool-input-schema tool)))
    (is (functionp (cl-mcp-server.tools:tool-handler tool)))))

(test list-tools-returns-all-tools
  "Test that list-tools returns all defined tools"
  (let ((tools (cl-mcp-server.tools:list-tools)))
    (is (listp tools))
    (is (>= (length tools) 4))
    ;; Check all expected tools exist
    (is (find "evaluate-lisp" tools
              :key #'cl-mcp-server.tools:tool-name
              :test #'string=))
    (is (find "list-definitions" tools
              :key #'cl-mcp-server.tools:tool-name
              :test #'string=))
    (is (find "reset-session" tools
              :key #'cl-mcp-server.tools:tool-name
              :test #'string=))
    (is (find "load-system" tools
              :key #'cl-mcp-server.tools:tool-name
              :test #'string=))))

(test get-tool-returns-correct-tool
  "Test that get-tool retrieves the correct tool by name"
  (let ((tool (cl-mcp-server.tools:get-tool "evaluate-lisp")))
    (is (string= "evaluate-lisp" (cl-mcp-server.tools:tool-name tool)))))

(test get-tool-unknown-returns-nil
  "Test that get-tool returns nil for unknown tools"
  (is (null (cl-mcp-server.tools:get-tool "nonexistent-tool"))))

;;; ==========================================================================
;;; Tool Schema Tests
;;; ==========================================================================

(test evaluate-lisp-schema
  "Test evaluate-lisp tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "evaluate-lisp"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
    (let ((required (cdr (assoc "required" schema :test #'string=))))
      (is (member "code" required :test #'string=)))))

(test list-definitions-schema
  "Test list-definitions tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "list-definitions"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
    ;; list-definitions has no required params
    (let ((required (cdr (assoc "required" schema :test #'string=))))
      (is (or (null required) (= 0 (length required)))))))

(test reset-session-schema
  "Test reset-session tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "reset-session"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))))

(test load-system-schema
  "Test load-system tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "load-system"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
    (let ((required (cdr (assoc "required" schema :test #'string=))))
      (is (member "system-name" required :test #'string=)))))

;;; ==========================================================================
;;; Tools JSON Formatting Tests
;;; ==========================================================================

(test tools-for-mcp-list
  "Test that tools can be formatted for MCP tools/list response"
  (let ((tools-json (cl-mcp-server.tools:tools-for-mcp)))
    (is (listp tools-json))
    (dolist (tool tools-json)
      (is (assoc "name" tool :test #'string=))
      (is (assoc "description" tool :test #'string=))
      (is (assoc "inputSchema" tool :test #'string=)))))

;;; ==========================================================================
;;; Tool Calling Tests
;;; ==========================================================================

(test call-tool-evaluate-lisp
  "Test calling evaluate-lisp tool"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "evaluate-lisp"
                     '(("code" . "(+ 1 2)"))
                     session)))
        (is (stringp result))
        (is (search "3" result))))))

(test call-tool-list-definitions
  "Test calling list-definitions tool"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "list-definitions"
                     nil
                     session)))
        (is (stringp result))))))

(test call-tool-reset-session
  "Test calling reset-session tool"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "reset-session"
                     nil
                     session)))
        (is (stringp result))
        (is (search "reset" result :test #'char-equal))))))

(test call-tool-unknown-signals-error
  "Test that calling unknown tool signals method-not-found"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (signals cl-mcp-server.conditions:method-not-found
        (cl-mcp-server.tools:call-tool "nonexistent-tool" nil session)))))

(test call-tool-missing-required-signals-error
  "Test that missing required args signals invalid-params"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (signals cl-mcp-server.conditions:invalid-params
        (cl-mcp-server.tools:call-tool "evaluate-lisp" nil session)))))

(test validate-tool-args-success
  "Test validate-tool-args with valid arguments"
  (let ((schema '(("type" . "object")
                  ("required" . ("code"))
                  ("properties" . (("code" . (("type" . "string"))))))))
    (is (cl-mcp-server.tools:validate-tool-args
         '(("code" . "(+ 1 2)"))
         schema))))

(test validate-tool-args-missing-required
  "Test validate-tool-args signals error for missing required"
  (let ((schema '(("type" . "object")
                  ("required" . ("code"))
                  ("properties" . (("code" . (("type" . "string"))))))))
    (signals cl-mcp-server.conditions:invalid-params
      (cl-mcp-server.tools:validate-tool-args nil schema))))

(test validate-tool-args-no-required
  "Test validate-tool-args with no required fields"
  (let ((schema '(("type" . "object")
                  ("required" . ())
                  ("properties" . ()))))
    (is (cl-mcp-server.tools:validate-tool-args nil schema))))

;;; ==========================================================================
;;; Configure-Limits Tool Tests
;;; ==========================================================================

(test configure-limits-exists
  "Test that configure-limits tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "configure-limits")))))

(test configure-limits-returns-current-config
  "Test that configure-limits returns current configuration"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "configure-limits"
                     nil
                     session)))
        (is (stringp result))
        (is (search "timeout" result))
        (is (search "max-output" result))))))

(test configure-limits-changes-timeout
  "Test that configure-limits can change timeout"
  (let ((session (cl-mcp-server.session:make-session))
        (original cl-mcp-server.evaluator:*evaluation-timeout*))
    (unwind-protect
         (cl-mcp-server.session:with-session (session)
           (cl-mcp-server.tools:call-tool
            "configure-limits"
            '(("timeout" . 60))
            session)
           (is (= 60 cl-mcp-server.evaluator:*evaluation-timeout*)))
      ;; Restore original
      (setf cl-mcp-server.evaluator:*evaluation-timeout* original))))

(test configure-limits-zero-disables-timeout
  "Test that setting timeout to 0 disables it"
  (let ((session (cl-mcp-server.session:make-session))
        (original cl-mcp-server.evaluator:*evaluation-timeout*))
    (unwind-protect
         (cl-mcp-server.session:with-session (session)
           (cl-mcp-server.tools:call-tool
            "configure-limits"
            '(("timeout" . 0))
            session)
           (is (null cl-mcp-server.evaluator:*evaluation-timeout*)))
      ;; Restore original
      (setf cl-mcp-server.evaluator:*evaluation-timeout* original))))

(test configure-limits-changes-max-output
  "Test that configure-limits can change max-output"
  (let ((session (cl-mcp-server.session:make-session))
        (original cl-mcp-server.evaluator:*max-output-chars*))
    (unwind-protect
         (cl-mcp-server.session:with-session (session)
           (cl-mcp-server.tools:call-tool
            "configure-limits"
            '(("max-output" . 50000))
            session)
           (is (= 50000 cl-mcp-server.evaluator:*max-output-chars*)))
      ;; Restore original
      (setf cl-mcp-server.evaluator:*max-output-chars* original))))

;;; ==========================================================================
;;; Phase B: Enhanced Evaluation Tool Tests
;;; ==========================================================================

(test evaluate-lisp-with-package-param
  "Test evaluate-lisp tool with package parameter"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "evaluate-lisp"
                     '(("code" . "*package*")
                       ("package" . "CL"))
                     session)))
        (is (stringp result))
        (is (search "COMMON-LISP" result))))))

(test evaluate-lisp-with-timing-param
  "Test evaluate-lisp tool with capture-time parameter"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "evaluate-lisp"
                     '(("code" . "(loop for i from 1 to 1000 sum i)")
                       ("capture-time" . t))
                     session)))
        (is (stringp result))
        (is (search "Timing" result))
        (is (search "ms" result))))))

;;; ==========================================================================
;;; Phase B: compile-form Tool Tests
;;; ==========================================================================

(test compile-form-tool-exists
  "Test that compile-form tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "compile-form")))))

(test compile-form-schema
  "Test compile-form tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "compile-form"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
    (let ((required (cdr (assoc "required" schema :test #'string=))))
      (is (member "code" required :test #'string=)))))

(test compile-form-valid-code
  "Test compile-form with valid code"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "compile-form"
                     '(("code" . "(+ 1 2 3)"))
                     session)))
        (is (stringp result))
        (is (search "successful" result))))))

(test compile-form-catches-type-error
  "Test compile-form catches type errors"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "compile-form"
                     '(("code" . "(+ 1 \"not a number\")"))
                     session)))
        (is (stringp result))
        (is (search "Warning" result))))))

(test compile-form-with-package
  "Test compile-form with package parameter"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "compile-form"
                     '(("code" . "(list 'test)")
                       ("package" . "CL-USER"))
                     session)))
        (is (stringp result))
        (is (search "successful" result))))))

;;; ==========================================================================
;;; Phase B: time-execution Tool Tests
;;; ==========================================================================

(test time-execution-tool-exists
  "Test that time-execution tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "time-execution")))))

(test time-execution-schema
  "Test time-execution tool has correct schema"
  (let* ((tool (cl-mcp-server.tools:get-tool "time-execution"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
    (let ((required (cdr (assoc "required" schema :test #'string=))))
      (is (member "code" required :test #'string=)))))

(test time-execution-returns-timing
  "Test time-execution returns timing information"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "time-execution"
                     '(("code" . "(loop for i from 1 to 10000 sum i)"))
                     session)))
        (is (stringp result))
        (is (search "Timing:" result))
        (is (search "Real time:" result))
        (is (search "Run time:" result))
        (is (search "GC time:" result))
        (is (search "Bytes consed:" result))
        (is (search "Result:" result))))))

(test time-execution-with-package
  "Test time-execution with package parameter"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "time-execution"
                     '(("code" . "*package*")
                       ("package" . "CL"))
                     session)))
        (is (stringp result))
        (is (search "COMMON-LISP" result))))))

(test time-execution-shows-result
  "Test time-execution includes the computed result"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "time-execution"
                     '(("code" . "(+ 100 200 300)"))
                     session)))
        (is (stringp result))
        (is (search "600" result))))))

;;; ==========================================================================
;;; Usage Guide Tool Tests
;;; ==========================================================================

(test get-usage-guide-tool-exists
  "Test that get-usage-guide tool is registered"
  (is (not (null (cl-mcp-server.tools:get-tool "get-usage-guide")))))

(test get-usage-guide-returns-markdown
  "Test get-usage-guide returns markdown documentation"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let ((result (cl-mcp-server.tools:call-tool
                     "get-usage-guide"
                     nil
                     session)))
        (is (stringp result))
        ;; Should have header
        (is (search "# CL-MCP-Server Usage Guide" result))
        ;; Should mention key tools
        (is (search "evaluate-lisp" result))
        (is (search "validate-syntax" result))
        ;; Should mention workflow
        (is (search "Validate Before Save" result))))))

(test get-usage-guide-no-args-required
  "Test get-usage-guide works without arguments"
  (let* ((tool (cl-mcp-server.tools:get-tool "get-usage-guide"))
         (schema (cl-mcp-server.tools:tool-input-schema tool)))
    ;; Should have no required args
    (is (null (cdr (assoc "required" schema :test #'string=))))))
