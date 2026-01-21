;;; tests/json-rpc-tests.lisp
;;; ABOUTME: Tests for JSON-RPC parsing and encoding

(in-package #:cl-mcp-server-tests)

(def-suite json-rpc-tests
  :description "JSON-RPC message tests"
  :in cl-mcp-server-tests)

(in-suite json-rpc-tests)

(test make-request
  "Can create JSON-RPC request objects"
  (let ((req (cl-mcp-server.json-rpc:make-request
               :id 1
               :method "test"
               :params '(("a" . 1)))))
    (is (= 1 (cl-mcp-server.json-rpc:request-id req)))
    (is (string= "test" (cl-mcp-server.json-rpc:request-method req)))
    (is (equal '(("a" . 1)) (cl-mcp-server.json-rpc:request-params req)))))

(test make-notification
  "Notifications have no ID"
  (let ((notif (cl-mcp-server.json-rpc:make-notification
                 :method "notify")))
    (is (null (cl-mcp-server.json-rpc:request-id notif)))
    (is (cl-mcp-server.json-rpc:notification-p notif))))

(test make-success-response
  "Can create success responses"
  (let ((resp (cl-mcp-server.json-rpc:make-success-response
                :id 1
                :result '(("value" . 42)))))
    (is (= 1 (cl-mcp-server.json-rpc:response-id resp)))
    (is (equal '(("value" . 42)) (cl-mcp-server.json-rpc:response-result resp)))
    (is (null (cl-mcp-server.json-rpc:response-error resp)))))

(test make-error-response
  "Can create error responses"
  (let ((resp (cl-mcp-server.json-rpc:make-error-response
                :id 1
                :code -32601
                :message "Method not found")))
    (is (= 1 (cl-mcp-server.json-rpc:response-id resp)))
    (is (= -32601 (getf (cl-mcp-server.json-rpc:response-error resp) :code)))
    (is (string= "Method not found"
                 (getf (cl-mcp-server.json-rpc:response-error resp) :message)))))
