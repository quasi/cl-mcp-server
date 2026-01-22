;;; tests/integration-tests.lisp
;;; ABOUTME: MCP request handler tests

(in-package #:cl-mcp-server-tests)

(def-suite integration-tests
  :description "MCP request handler tests"
  :in cl-mcp-server-tests)

(in-suite integration-tests)

;;; ==========================================================================
;;; Handle Initialize Tests
;;; ==========================================================================

(test handle-initialize-returns-server-info
  "Test handle-initialize returns server info"
  (let ((response (cl-mcp-server::handle-initialize 1)))
    (is (not (null response)))
    (is (= 1 (cl-mcp-server.json-rpc:response-id response)))
    (is (null (cl-mcp-server.json-rpc:response-error response)))
    (let ((result (cl-mcp-server.json-rpc:response-result response)))
      (is (listp result))
      (is (assoc "serverInfo" result :test #'string=))
      (is (assoc "capabilities" result :test #'string=)))))

(test handle-initialize-includes-tools-capability
  "Test initialize response includes tools capability"
  (let* ((response (cl-mcp-server::handle-initialize 1))
         (result (cl-mcp-server.json-rpc:response-result response))
         (capabilities (cdr (assoc "capabilities" result :test #'string=))))
    (is (assoc "tools" capabilities :test #'string=))))

;;; ==========================================================================
;;; Handle Tools/List Tests
;;; ==========================================================================

(test handle-tools-list-returns-tools
  "Test handle-tools-list returns tool list"
  (let ((response (cl-mcp-server::handle-tools-list 2)))
    (is (not (null response)))
    (is (= 2 (cl-mcp-server.json-rpc:response-id response)))
    (is (null (cl-mcp-server.json-rpc:response-error response)))
    (let ((result (cl-mcp-server.json-rpc:response-result response)))
      (is (listp result))
      (is (assoc "tools" result :test #'string=))
      (let ((tools (cdr (assoc "tools" result :test #'string=))))
        (is (>= (length tools) 4))))))

(test handle-tools-list-tools-have-correct-format
  "Test tools in list have name, description, inputSchema"
  (let* ((response (cl-mcp-server::handle-tools-list 1))
         (result (cl-mcp-server.json-rpc:response-result response))
         (tools (cdr (assoc "tools" result :test #'string=))))
    (dolist (tool tools)
      (is (assoc "name" tool :test #'string=))
      (is (assoc "description" tool :test #'string=))
      (is (assoc "inputSchema" tool :test #'string=)))))

;;; ==========================================================================
;;; Handle Tools/Call Tests
;;; ==========================================================================

(test handle-tools-call-evaluate-lisp
  "Test handle-tools-call with evaluate-lisp"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((params '(("name" . "evaluate-lisp")
                       ("arguments" . (("code" . "(+ 1 2)")))))
             (response (cl-mcp-server::handle-tools-call 3 params)))
        (is (not (null response)))
        (is (= 3 (cl-mcp-server.json-rpc:response-id response)))
        (is (null (cl-mcp-server.json-rpc:response-error response)))
        (let* ((result (cl-mcp-server.json-rpc:response-result response))
               (content (cdr (assoc "content" result :test #'string=)))
               (text-item (find "text" content
                                :key (lambda (x) (cdr (assoc "type" x :test #'string=)))
                                :test #'string=)))
          (is (not (null text-item)))
          (is (search "3" (cdr (assoc "text" text-item :test #'string=)))))))))

(test handle-tools-call-unknown-tool
  "Test handle-tools-call with unknown tool returns error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((params '(("name" . "nonexistent-tool")
                       ("arguments" . ())))
             (response (cl-mcp-server::handle-tools-call 4 params)))
        (is (not (null response)))
        (is (= 4 (cl-mcp-server.json-rpc:response-id response)))
        (is (not (null (cl-mcp-server.json-rpc:response-error response))))
        (is (= -32601 (getf (cl-mcp-server.json-rpc:response-error response) :code)))))))

(test handle-tools-call-missing-required
  "Test handle-tools-call with missing required args returns error"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((params '(("name" . "evaluate-lisp")
                       ("arguments" . ())))
             (response (cl-mcp-server::handle-tools-call 5 params)))
        (is (not (null response)))
        (is (= 5 (cl-mcp-server.json-rpc:response-id response)))
        (is (not (null (cl-mcp-server.json-rpc:response-error response))))
        (is (= -32602 (getf (cl-mcp-server.json-rpc:response-error response) :code)))))))

;;; ==========================================================================
;;; Handle Request Dispatch Tests
;;; ==========================================================================

(test handle-request-initialize
  "Test handle-request dispatches initialize"
  (let* ((request (cl-mcp-server.json-rpc:make-request
                   :id 1
                   :method "initialize"
                   :params nil))
         (response (cl-mcp-server::handle-request request)))
    (is (not (null response)))
    (is (= 1 (cl-mcp-server.json-rpc:response-id response)))
    (is (null (cl-mcp-server.json-rpc:response-error response)))))

(test handle-request-tools-list
  "Test handle-request dispatches tools/list"
  (let* ((request (cl-mcp-server.json-rpc:make-request
                   :id 2
                   :method "tools/list"
                   :params nil))
         (response (cl-mcp-server::handle-request request)))
    (is (not (null response)))
    (is (= 2 (cl-mcp-server.json-rpc:response-id response)))
    (is (null (cl-mcp-server.json-rpc:response-error response)))))

(test handle-request-tools-call
  "Test handle-request dispatches tools/call"
  (let ((session (cl-mcp-server.session:make-session)))
    (cl-mcp-server.session:with-session (session)
      (let* ((request (cl-mcp-server.json-rpc:make-request
                       :id 3
                       :method "tools/call"
                       :params '(("name" . "evaluate-lisp")
                                 ("arguments" . (("code" . "(+ 1 2)"))))))
             (response (cl-mcp-server::handle-request request)))
        (is (not (null response)))
        (is (= 3 (cl-mcp-server.json-rpc:response-id response)))
        (is (null (cl-mcp-server.json-rpc:response-error response)))))))

(test handle-request-unknown-method
  "Test handle-request returns error for unknown method"
  (let* ((request (cl-mcp-server.json-rpc:make-request
                   :id 4
                   :method "unknown/method"
                   :params nil))
         (response (cl-mcp-server::handle-request request)))
    (is (not (null response)))
    (is (= 4 (cl-mcp-server.json-rpc:response-id response)))
    (is (not (null (cl-mcp-server.json-rpc:response-error response))))
    (is (= -32601 (getf (cl-mcp-server.json-rpc:response-error response) :code)))))

(test handle-request-notification-returns-nil
  "Test handle-request returns nil for notifications"
  (let* ((request (cl-mcp-server.json-rpc:make-notification
                   :method "notifications/initialized"
                   :params nil))
         (response (cl-mcp-server::handle-request request)))
    (is (null response))))

;;; ==========================================================================
;;; Server Full Session Tests
;;; ==========================================================================

(test server-full-session
  "Test a full MCP session from initialize through tool calls"
  (let ((input (make-string-input-stream
                (format nil "~{~a~%~}"
                        '("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
                          "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
                          "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}"
                          "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"evaluate-lisp\",\"arguments\":{\"code\":\"(+ 1 2)\"}}}"))))
        (output (make-string-output-stream)))
    (cl-mcp-server:run-server :input input :output output)
    (let* ((output-str (get-output-stream-string output))
           (lines (remove-if #'zerop
                             (mapcar #'length
                                     (split-by-newline output-str)))))
      ;; Should have 3 responses (initialize, tools/list, tools/call)
      ;; Notification doesn't generate a response
      (is (= 3 (length lines))))))

(defun split-by-newline (string)
  "Split STRING by newlines."
  (loop with start = 0
        for end = (position #\Newline string :start start)
        collect (subseq string start (or end (length string)))
        while end
        do (setf start (1+ end))))
