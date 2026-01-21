;;; tests/encoding-tests.lisp
;;; ABOUTME: Tests for JSON-RPC response encoding

(in-package #:cl-mcp-server-tests)

(def-suite encoding-tests
  :description "JSON-RPC encoding tests"
  :in cl-mcp-server-tests)

(in-suite encoding-tests)

(test encode-success-response
  "Encode success response to JSON"
  (let* ((resp (cl-mcp-server.json-rpc:make-success-response
                 :id 1
                 :result '(("value" . 42))))
         (json (cl-mcp-server.json-rpc:encode-response resp))
         (parsed (yason:parse json :object-as :alist)))
    (is (string= "2.0" (cdr (assoc "jsonrpc" parsed :test #'string=))))
    (is (= 1 (cdr (assoc "id" parsed :test #'string=))))
    (is (= 42 (cdr (assoc "value" (cdr (assoc "result" parsed :test #'string=))
                          :test #'string=))))
    ;; Must not have error field
    (is (not (assoc "error" parsed :test #'string=)))))

(test encode-error-response
  "Encode error response to JSON"
  (let* ((resp (cl-mcp-server.json-rpc:make-error-response
                 :id 1
                 :code -32601
                 :message "Method not found"))
         (json (cl-mcp-server.json-rpc:encode-response resp))
         (parsed (yason:parse json :object-as :alist)))
    (is (string= "2.0" (cdr (assoc "jsonrpc" parsed :test #'string=))))
    (is (= 1 (cdr (assoc "id" parsed :test #'string=))))
    ;; Must not have result field
    (is (not (assoc "result" parsed :test #'string=)))
    (let ((err (cdr (assoc "error" parsed :test #'string=))))
      (is (= -32601 (cdr (assoc "code" err :test #'string=))))
      (is (string= "Method not found" (cdr (assoc "message" err :test #'string=)))))))

(test encode-null-id
  "Null id encodes as JSON null"
  (let* ((resp (cl-mcp-server.json-rpc:make-error-response
                 :id nil
                 :code -32700
                 :message "Parse error"))
         (json (cl-mcp-server.json-rpc:encode-response resp)))
    (is (search "\"id\":null" json))))
