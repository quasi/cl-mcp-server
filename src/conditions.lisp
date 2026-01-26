;;; src/conditions.lisp
;;; ABOUTME: MCP and JSON-RPC condition definitions

(in-package #:cl-mcp-server.conditions)

;;; Base condition

(define-condition mcp-error (error)
  ((message :initarg :message :reader error-message :initform ""))
  (:documentation "Base condition for MCP errors"))

;;; JSON-RPC 2.0 standard error conditions

(define-condition json-rpc-error (mcp-error)
  ((code :reader error-code :allocation :class)
   (data :initarg :data :reader error-data :initform nil))
  (:documentation "Base condition for JSON-RPC errors"))

(define-condition parse-error (json-rpc-error)
  ((code :initform -32700 :allocation :class))
  (:report (lambda (c s)
             (format s "Parse error: ~a" (error-message c))))
  (:documentation "Invalid JSON was received"))

(define-condition invalid-request (json-rpc-error)
  ((code :initform -32600 :allocation :class))
  (:report (lambda (c s)
             (format s "Invalid Request: ~a" (error-message c))))
  (:documentation "The JSON sent is not a valid Request object"))

(define-condition method-not-found (json-rpc-error)
  ((code :initform -32601 :allocation :class))
  (:report (lambda (c s)
             (format s "Method not found: ~a" (error-message c))))
  (:documentation "The method does not exist / is not available"))

(define-condition invalid-params (json-rpc-error)
  ((code :initform -32602 :allocation :class))
  (:report (lambda (c s)
             (format s "Invalid params: ~a" (error-message c))))
  (:documentation "Invalid method parameter(s)"))

(define-condition internal-error (json-rpc-error)
  ((code :initform -32603 :allocation :class))
  (:report (lambda (c s)
             (format s "Internal error: ~a" (error-message c))))
  (:documentation "Internal JSON-RPC error"))

;;; Evaluation conditions

(define-condition evaluation-timeout (mcp-error)
  ((timeout-seconds
    :initarg :timeout-seconds
    :reader timeout-seconds
    :documentation "The timeout duration that was exceeded")
   (backtrace
    :initarg :backtrace
    :reader timeout-backtrace
    :initform nil
    :documentation "Stack trace captured at timeout"))
  (:report (lambda (c s)
             (format s "Evaluation exceeded ~A second timeout~@[~%~%Backtrace:~%~A~]"
                     (timeout-seconds c)
                     (timeout-backtrace c))))
  (:documentation "Signaled when code evaluation exceeds the configured timeout"))
