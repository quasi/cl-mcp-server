;;; src/json-rpc.lisp
;;; ABOUTME: JSON-RPC 2.0 message parsing and encoding

(in-package #:cl-mcp-server.json-rpc)

;;; Message structures

(defstruct (json-rpc-request (:conc-name request-))
  "A JSON-RPC 2.0 request or notification"
  (id nil)           ; nil for notifications
  (method "" :type string)
  (params nil))      ; alist or list

(defstruct (json-rpc-response (:conc-name response-))
  "A JSON-RPC 2.0 response"
  (id nil)
  (result nil)
  (error nil))       ; plist (:code :message :data)

;;; Constructors

(defun make-request (&key id method params)
  "Create a JSON-RPC request"
  (make-json-rpc-request :id id :method method :params params))

(defun make-notification (&key method params)
  "Create a JSON-RPC notification (no id)"
  (make-json-rpc-request :id nil :method method :params params))

(defun notification-p (request)
  "True if request is a notification (no id)"
  (null (request-id request)))

(defun make-success-response (&key id result)
  "Create a successful JSON-RPC response"
  (make-json-rpc-response :id id :result result :error nil))

(defun make-error-response (&key id code message data)
  "Create an error JSON-RPC response"
  (make-json-rpc-response
    :id id
    :result nil
    :error (list :code code :message message :data data)))

;;; Parsing

(defun parse-message (json-string)
  "Parse a JSON-RPC message from a JSON string.
   Signals parse-error for invalid JSON.
   Signals invalid-request for protocol violations.
   Returns a json-rpc-request struct."
  (let ((data (handler-case
                  (yason:parse json-string :object-as :alist)
                (error (e)
                  (error 'parse-error
                         :message (format nil "~a" e))))))
    (validate-and-build-request data)))

(defun validate-and-build-request (data)
  "Validate parsed JSON data and build request struct"
  (unless (assoc "jsonrpc" data :test #'string=)
    (error 'invalid-request :message "missing jsonrpc field"))
  (unless (string= "2.0" (cdr (assoc "jsonrpc" data :test #'string=)))
    (error 'invalid-request :message "jsonrpc must be \"2.0\""))
  (unless (assoc "method" data :test #'string=)
    (error 'invalid-request :message "missing method field"))
  (let ((method (cdr (assoc "method" data :test #'string=))))
    (unless (stringp method)
      (error 'invalid-request :message "method must be a string")))
  (make-json-rpc-request
    :id (cdr (assoc "id" data :test #'string=))
    :method (cdr (assoc "method" data :test #'string=))
    :params (cdr (assoc "params" data :test #'string=))))

;;; Encoding

(defun alistp (obj)
  "Check if OBJ is an alist (list of (string . value) pairs)."
  (and (listp obj)
       (every (lambda (pair)
                (and (consp pair)
                     (stringp (car pair))))
              obj)))

(defun convert-for-json (value)
  "Recursively convert VALUE for JSON encoding.
Alists become hash tables, lists of alists become lists of hash tables."
  (cond
    ;; Null stays null
    ((null value) value)
    ;; Alist -> hash table
    ((alistp value)
     (let ((ht (make-hash-table :test #'equal)))
       (dolist (pair value ht)
         (setf (gethash (car pair) ht)
               (convert-for-json (cdr pair))))))
    ;; List (but not alist) -> convert each element
    ((listp value)
     (mapcar #'convert-for-json value))
    ;; Atoms stay as-is
    (t value)))

(defun encode-response (response)
  "Encode a JSON-RPC response to a JSON string.
   Returns a string with no trailing newline."
  (with-output-to-string (s)
    (let ((ht (make-hash-table :test #'equal)))
      (setf (gethash "jsonrpc" ht) "2.0")
      (setf (gethash "id" ht) (response-id response))
      (if (response-error response)
          ;; Error response
          (let ((err-ht (make-hash-table :test #'equal)))
            (setf (gethash "code" err-ht) (getf (response-error response) :code))
            (setf (gethash "message" err-ht) (getf (response-error response) :message))
            (when (getf (response-error response) :data)
              (setf (gethash "data" err-ht) (getf (response-error response) :data)))
            (setf (gethash "error" ht) err-ht))
          ;; Success response - recursively convert result
          (setf (gethash "result" ht)
                (convert-for-json (response-result response))))
      (yason:encode ht s))))

(defun encode-error (condition &optional id)
  "Encode a json-rpc-error condition as a JSON response string"
  (encode-response
    (make-error-response
      :id id
      :code (error-code condition)
      :message (error-message condition)
      :data (when (slot-boundp condition 'data)
              (error-data condition)))))
