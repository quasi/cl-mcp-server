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
