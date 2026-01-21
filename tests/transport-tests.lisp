;;; tests/transport-tests.lisp
;;; ABOUTME: Tests for stdio transport

(in-package #:cl-mcp-server-tests)

(def-suite transport-tests
  :description "Transport layer tests"
  :in cl-mcp-server-tests)

(in-suite transport-tests)

(test read-message-from-stream
  "Read a single JSON-RPC message from stream"
  (let* ((input (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\"}~%"))
         (stream (make-string-input-stream input)))
    (let ((msg (cl-mcp-server.transport:read-message stream)))
      (is (= 1 (cl-mcp-server.json-rpc:request-id msg)))
      (is (string= "test" (cl-mcp-server.json-rpc:request-method msg))))))

(test write-message-to-stream
  "Write a JSON-RPC response to stream"
  (let* ((resp (cl-mcp-server.json-rpc:make-success-response :id 1 :result t))
         (output (make-string-output-stream)))
    (cl-mcp-server.transport:write-message resp output)
    (let ((result (get-output-stream-string output)))
      ;; Should end with newline
      (is (char= #\Newline (char result (1- (length result)))))
      ;; Should be valid JSON
      (is (yason:parse result)))))

(test read-multiple-messages
  "Read multiple messages in sequence"
  (let* ((input (format nil "~a~%~a~%"
                        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"first\"}"
                        "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"second\"}"))
         (stream (make-string-input-stream input)))
    (let ((msg1 (cl-mcp-server.transport:read-message stream))
          (msg2 (cl-mcp-server.transport:read-message stream)))
      (is (string= "first" (cl-mcp-server.json-rpc:request-method msg1)))
      (is (string= "second" (cl-mcp-server.json-rpc:request-method msg2))))))

(test read-message-eof
  "EOF returns nil"
  (let ((stream (make-string-input-stream "")))
    (is (null (cl-mcp-server.transport:read-message stream)))))
